// vim:sw=4:ts=4:et:
#define _POSIX_C_SOURCE 200809L
#define WITH_XINERAMA 1
#include <vector>
#include <memory>
#include <cmath>
#include <string>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <poll.h>
#include <getopt.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <xcb/xcb.h>
#include <xcb/xcbext.h>
#if WITH_XINERAMA
#include <xcb/xinerama.h>
#endif
#include <xcb/randr.h>

// Here be dragons

#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))
#define indexof(c,s) (strchr((s),(c))-(s))

struct font_t {
  xcb_font_t ptr;
  int descent, height, width;
  uint16_t char_max;
  uint16_t char_min;
  std::vector<xcb_charinfo_t> width_lut;
};
using font_p = std::unique_ptr<font_t>;

struct monitor_t {
  std::string name;
  uint32_t x, y, width, height;
  xcb_window_t window;
  xcb_pixmap_t pixmap;
};

struct area_t {
  unsigned begin;
  unsigned end;
  bool complete;
  unsigned align;
  unsigned button;
  xcb_window_t window;
  char *cmd;
};

union rgba_t {
  struct {
    uint8_t b;
    uint8_t g;
    uint8_t r;
    uint8_t a;
  };
  uint32_t v;
  rgba_t(uint8_t r, uint8_t g, uint8_t b, uint8_t a) : b(b), g(g), r(r), a(a) {}
  rgba_t() {}
  rgba_t(uint32_t v) : v(v) {}
};

enum {
  ATTR_OVERL = (1<<0),
  ATTR_UNDERL = (1<<1),
};

enum {
  ALIGN_L = 0,
  ALIGN_C,
  ALIGN_R
};

enum {
  GC_DRAW = 0,
  GC_CLEAR,
  GC_ATTR,
  GC_MAX
};

static xcb_connection_t *c;
static xcb_screen_t *scr;
static xcb_gcontext_t gc[GC_MAX];
static xcb_visualid_t visual;
static xcb_colormap_t colormap;
static std::vector<monitor_t> mon_list;
static std::vector<font_p> font_list;
static int font_index = -1;
static uint32_t attrs = 0;
static bool dock = false;
static bool topbar = true;
static int16_t bw = -1, bh = -1, bx = 0, by = 0;
static int bu = 1; // Underline height
static rgba_t fgc, bgc, ugc;
static rgba_t dfgc, dbgc, dugc;
static std::vector<area_t> area_stack;

static const rgba_t BLACK = rgba_t(0, 0, 0, 255);
static const rgba_t WHITE = rgba_t(255, 255, 255, 255);

static std::vector<std::string> output_names;

char*
_memrchr(char* s, int c, size_t n) {
  char* ptr = s + n - 1;
  for(; ptr >= s; ptr--) {
    if (*ptr == c)
      return ptr;
  }
  return nullptr;
}

void
update_gc ()
{
  xcb_change_gc(c, gc[GC_DRAW], XCB_GC_FOREGROUND, &fgc.v);
  xcb_change_gc(c, gc[GC_CLEAR], XCB_GC_FOREGROUND, &bgc.v);
  xcb_change_gc(c, gc[GC_ATTR], XCB_GC_FOREGROUND, &ugc.v);
}

void
fill_gradient (xcb_drawable_t d, int16_t x, int16_t y, uint16_t width, uint16_t height, rgba_t start, rgba_t stop)
{
  const uint8_t K = 25; // The number of steps

  for (uint8_t i = 0; i < K; i++) {
    // Perform the linear interpolation magic
    uint8_t rr = (i * stop.r + (K - i) * start.r) / K;
    uint8_t gg = (i * stop.g + (K - i) * start.g) / K;
    uint8_t bb = (i * stop.b + (K - i) * start.b) / K;

    // The alpha is ignored here
    rgba_t step(rr, gg, bb, 255);

    xcb_change_gc(c, gc[GC_DRAW], XCB_GC_FOREGROUND, &step.v);
    xcb_rectangle_t rect{ x, int16_t(i * bh / K), width, uint16_t(bh / K + 1) };
    xcb_poly_fill_rectangle(c, d, gc[GC_DRAW], 1, &rect);
  }

  xcb_change_gc(c, gc[GC_DRAW], XCB_GC_FOREGROUND, &fgc.v);
}

void
fill_rect (xcb_drawable_t d, xcb_gcontext_t _gc, int16_t x, int16_t y, uint16_t width, uint16_t height)
{
  xcb_rectangle_t rect{ x, y, width, height };
  xcb_poly_fill_rectangle(c, d, _gc, 1, &rect);
}

// Apparently xcb cannot seem to compose the right request for this call, hence we have to do it by
// ourselves.
// The funcion is taken from 'wmdia' (http://wmdia.sourceforge.net/)
xcb_void_cookie_t xcb_poly_text_16_simple(xcb_connection_t * c,
  xcb_drawable_t drawable, xcb_gcontext_t gc, int16_t x, int16_t y,
  uint32_t len, const uint16_t *str)
{
  static const xcb_protocol_request_t xcb_req = {
    5,        // count
    0,        // ext
    XCB_POLY_TEXT_16, // opcode
    1         // isvoid
  };
  struct iovec xcb_parts[7];
  uint8_t xcb_lendelta[2];
  xcb_void_cookie_t xcb_ret;
  xcb_poly_text_8_request_t xcb_out;

  xcb_out.pad0 = 0;
  xcb_out.drawable = drawable;
  xcb_out.gc = gc;
  xcb_out.x = x;
  xcb_out.y = y;

  xcb_lendelta[0] = len;
  xcb_lendelta[1] = 0;

  xcb_parts[2].iov_base = (char *)&xcb_out;
  xcb_parts[2].iov_len = sizeof(xcb_out);
  xcb_parts[3].iov_base = 0;
  xcb_parts[3].iov_len = -xcb_parts[2].iov_len & 3;

  xcb_parts[4].iov_base = xcb_lendelta;
  xcb_parts[4].iov_len = sizeof(xcb_lendelta);
  xcb_parts[5].iov_base = (char *)str;
  xcb_parts[5].iov_len = len * sizeof(int16_t);

  xcb_parts[6].iov_base = 0;
  xcb_parts[6].iov_len = -(xcb_parts[4].iov_len + xcb_parts[5].iov_len) & 3;

  xcb_ret.sequence = xcb_send_request(c, 0, xcb_parts + 2, &xcb_req);

  return xcb_ret;
}

int
shift (monitor_t& mon, int x, int align, int ch_width)
{
  switch (align) {
    case ALIGN_C:
      xcb_copy_area(c, mon.pixmap, mon.pixmap, gc[GC_DRAW],
          mon.width / 2 - x / 2, 0,
          mon.width / 2 - (x + ch_width) / 2, 0,
          x, bh);
      x = mon.width / 2 - (x + ch_width) / 2 + x;
      break;
    case ALIGN_R:
      xcb_copy_area(c, mon.pixmap, mon.pixmap, gc[GC_DRAW],
          mon.width - x, 0,
          mon.width - x - ch_width, 0,
          x, bh);
      x = mon.width - ch_width;
      break;
  }

  // Draw the background first
  fill_rect(mon.pixmap, gc[GC_CLEAR], x, 0, ch_width, bh);
  return x;
}

void
draw_lines (monitor_t& mon, int x, int w)
{
  /* We can render both at the same time */
  if (attrs & ATTR_OVERL)
    fill_rect(mon.pixmap, gc[GC_ATTR], x, 0, w, bu);
  if (attrs & ATTR_UNDERL)
    fill_rect(mon.pixmap, gc[GC_ATTR], x, bh - bu, w, bu);
}

void
draw_shift (monitor_t& mon, int x, int align, int w)
{
  x = shift(mon, x, align, w);
  draw_lines(mon, x, w);
}

int
draw_char (monitor_t& mon, font_t& cur_font, int x, int align, uint16_t ch)
{
  int ch_width = (!cur_font.width_lut.empty()) ?
    cur_font.width_lut[ch - cur_font.char_min].character_width:
    cur_font.width;

  x = shift(mon, x, align, ch_width);

  // xcb accepts string in UCS-2 BE, so swap
  ch = (ch >> 8) | (ch << 8);

  // The coordinates here are those of the baseline
  xcb_poly_text_16_simple(c, mon.pixmap, gc[GC_DRAW],
              x, bh / 2 + cur_font.height / 2 - cur_font.descent,
              1, &ch);

  draw_lines(mon, x, ch_width);

  return ch_width;
}

rgba_t
parse_color (const char *str, char **end, const rgba_t def)
{
  int string_len;
  char *ep;

  if (!str)
    return def;

  // Reset
  if (str[0] == '-') {
    if (end)
      *end = (char *)str + 1;

    return def;
  }

  // Hex representation
  if (str[0] != '#') {
    if (end)
      *end = (char *)str;

    fprintf(stderr, "Invalid color specified\n");
    return def;
  }

  errno = 0;
  rgba_t tmp = rgba_t(strtoul(str + 1, &ep, 16));

  if (end)
    *end = ep;

  // Some error checking is definitely good
  if (errno) {
    fprintf(stderr, "Invalid color specified\n");
    return def;
  }

  string_len = ep - (str + 1);

  switch (string_len) {
    case 3:
      // Expand the #rgb format into #rrggbb (aa is set to 0xff)
      tmp.v = (tmp.v & 0xf00) * 0x1100
          | (tmp.v & 0x0f0) * 0x0110
          | (tmp.v & 0x00f) * 0x0011;
    case 6:
      // If the code is in #rrggbb form then assume it's opaque
      tmp.a = 255;
      break;
    case 7:
    case 8:
      // Colors in #aarrggbb format, those need no adjustments
      break;
    default:
      fprintf(stderr, "Invalid color specified\n");
      return def;
  }

  // Premultiply the alpha in
  if (tmp.a) {
    // The components are clamped automagically as the rgba_t is made of uint8_t
    return rgba_t(
      uint8_t((tmp.r * tmp.a) / 255),
      uint8_t((tmp.g * tmp.a) / 255),
      uint8_t((tmp.b * tmp.a) / 255),
      tmp.a
    );
  }

  return rgba_t(0U);
}

void
set_attribute (const char modifier, const char attribute)
{
  int pos = indexof(attribute, "ou");

  if (pos < 0) {
    fprintf(stderr, "Invalid attribute \"%c\" found\n", attribute);
    return;
  }

  switch (modifier) {
    case '+': attrs |= (1<<pos); break;
    case '-': attrs &=~(1<<pos); break;
    case '!': attrs ^= (1<<pos); break;
  }
}


area_t *
area_get (xcb_window_t win, const int btn, const int x)
{
  // Looping backwards ensures that we get the innermost area first
  for (int i = area_stack.size(); i --> 0;) {
    area_t *a = &area_stack[i];
    if (a->window == win && a->button == btn && x >= a->begin && x < a->end)
      return a;
  }
  return nullptr;
}

void
area_shift (xcb_window_t win, const int align, int delta)
{
  if (align == ALIGN_L)
    return;
  if (align == ALIGN_C)
    delta /= 2;

  for (int i = 0; i < area_stack.size(); i++) {
    area_t *a = &area_stack[i];
    if (a->window == win && a->align == align && !a->complete) {
      a->begin -= delta;
      a->end -= delta;
    }
  }
}

bool
area_add (char *str, const char *optend, char **end, monitor_t& mon, const int x, const int align, const int button)
{
  int i;
  char *trail;
  area_t *a;

  // A wild close area tag appeared!
  if (*str != ':') {
    *end = str;

    // Find most recent unclosed area.
    for (i = area_stack.size(); i --> 0 && !area_stack[i].complete; i--)
      ;
    a = &area_stack[i];

    // Basic safety checks
    if (!a->cmd || a->align != align || a->window != mon.window) {
      fprintf(stderr, "Invalid geometry for the clickable area\n");
      return false;
    }

    const int size = x - a->begin;

    switch (align) {
      case ALIGN_L:
        a->end = x;
        break;
      case ALIGN_C:
        a->begin = mon.width / 2 - size / 2 + a->begin / 2;
        a->end = a->begin + size;
        break;
      case ALIGN_R:
        // The newest is the rightmost one
        a->begin = mon.width - size;
        a->end = mon.width;
        break;
    }

    a->complete = false;
    return true;
  }

  a = &area_stack.emplace_back();

  // Found the closing : and check if it's just an escaped one
  for (trail = strchr(++str, ':'); trail && trail[-1] == '\\'; trail = strchr(trail + 1, ':'))
    ;

  // Find the trailing : and make sure it's within the formatting block, also reject empty commands
  if (!trail || str == trail || trail > optend) {
    *end = str;
    return false;
  }

  *trail = '\0';

  // Sanitize the user command by unescaping all the :
  for (char *needle = str; *needle; needle++) {
    int delta = trail - &needle[1];
    if (needle[0] == '\\' && needle[1] == ':') {
      memmove(&needle[0], &needle[1], delta);
      needle[delta] = 0;
    }
  }

  // This is a pointer to the string buffer allocated in the main
  a->cmd = str;
  a->complete = true;
  a->align = align;
  a->begin = x;
  a->window = mon.window;
  a->button = button;

  *end = trail + 1;

  return true;
}

bool
font_has_glyph (font_t& font, const uint16_t c)
{
  if (c < font.char_min || c > font.char_max)
    return false;

  if (!font.width_lut.empty() && font.width_lut[c - font.char_min].character_width == 0)
    return false;

  return true;
}

// returns nullptr if character cannot be printed
font_t*
select_drawable_font (const uint16_t c)
{
  // If the user has specified a font to use, try that first.
  if (font_index != -1 && font_has_glyph(*font_list[font_index - 1], c))
    return font_list[font_index - 1].get();

  // If the end is reached without finding an appropriate font, return nullptr.
  // If the font can draw the character, return it.
  for (int i = 0; i < font_list.size(); i++) {
    if (font_has_glyph(*font_list[i], c))
      return font_list[i].get();
  }
  return nullptr;
}

int
pos_to_absolute(monitor_t& mon, int pos, int align)
{
  switch (align) {
    case ALIGN_L: return pos;
    case ALIGN_R: return mon.width - pos;
    case ALIGN_C: return mon.width / 2 + pos / 2;
  }

  return 0;
}

void
parse (char *text)
{
  int pos_x, align, button;
  char *p = text, *block_end, *ep;

  pos_x = 0;
  align = ALIGN_L;
  auto cur_mon = mon_list.begin();

  // Reset the default color set
  bgc = dbgc;
  fgc = dfgc;
  ugc = fgc;
  update_gc();
  // Reset the default attributes
  attrs = 0;

  // Reset the stack position
  area_stack.clear();

  for (auto& m : mon_list)
    fill_rect(m.pixmap, gc[GC_CLEAR], 0, 0, m.width, bh);

  for (;;) {
    if (*p == '\0' || *p == '\n')
      return;

    if (p[0] == '%' && p[1] == '{' && (block_end = strchr(p++, '}'))) {
      p++;
      while (p < block_end) {
        while (isspace(*p))
          p++;

        switch (*p++) {
          // Enable/disable attributes.
          case '+': set_attribute('+', *p++); break;
          case '-': set_attribute('-', *p++); break;
          case '!': set_attribute('!', *p++); break;

          // Reverse foreground/background color.
          case 'R': {
            rgba_t tmp = fgc;
            fgc = bgc;
            bgc = tmp;
            update_gc();
          } break;

          // Alignment specifiers.
          // Keep track of where we are and where we're moving to so
          // that underlines/overlines are correctly drawn over the
          // empty space.
          case 'l': {
            int left_ep = 0;
            int right_ep = pos_to_absolute(*cur_mon, pos_x, align);
            draw_lines(*cur_mon, left_ep, right_ep - left_ep);
            pos_x = 0; align = ALIGN_L;
          } break;
          case 'c': {
            int left_ep = pos_to_absolute(*cur_mon, pos_x, align);
            int right_ep = cur_mon->width / 2;
            if (right_ep < left_ep) {
              int tmp = left_ep;
              left_ep = right_ep;
              right_ep = tmp;
            }
            draw_lines(*cur_mon, left_ep, right_ep - left_ep);
            pos_x = 0; align = ALIGN_C;
          } break;
          case 'r': {
            int left_ep = pos_to_absolute(*cur_mon, pos_x, align);
            int right_ep = cur_mon->width;
            if (right_ep < left_ep) {
              int tmp = left_ep;
              left_ep = right_ep;
              right_ep = tmp;
            }
            draw_lines(*cur_mon, left_ep, right_ep - left_ep);
            pos_x = 0; align = ALIGN_R;
          } break;

          // Define input area.
          case 'A': {
            button = XCB_BUTTON_INDEX_1;
            // The range is 1-5
            if (isdigit(*p) && (*p > '0' && *p < '6'))
              button = *p++ - '0';
            if (!area_add(p, block_end, &p, *cur_mon, pos_x, align, button))
              return;
          } break;

          // Set background/foreground/underline color.
          case 'B': bgc = parse_color(p, &p, dbgc); update_gc(); break;
          case 'F': fgc = parse_color(p, &p, dfgc); update_gc(); break;
          case 'U': ugc = parse_color(p, &p, dugc); update_gc(); break;

          // Set current monitor used for drawing.
          case 'S': {
            auto orig_mon = cur_mon;

            switch (*p) {
              case '+': // Next monitor.
                if (cur_mon != mon_list.end() - 1) cur_mon++;
                p += 1;
                break;
              case '-': // Previous monitor.
                if (cur_mon != mon_list.begin()) cur_mon--;
                p += 1;
                break;
              case 'f': // First monitor.
                cur_mon = mon_list.begin();
                p += 1;
                break;
              case 'l': // Last monitor.
                cur_mon = mon_list.end() - 1;
                p += 1;
                break;
              case 'n': { // Named monitor.
                const size_t name_len = block_end - (p + 1);
                for(cur_mon = mon_list.begin(); cur_mon != mon_list.end(); cur_mon++) {
                  if (cur_mon->name == p + 1)
                    break;
                }
                p += 1 + name_len;
              } break;
              case '0' ... '9': // Numbered monitor.
                cur_mon = mon_list.begin();
                for (int i = 0; i != *p-'0' && cur_mon != mon_list.end(); i++)
                  cur_mon++;
                p += 1;
                break;
              default:
                fprintf(stderr, "Unknown S specifier '%c'\n", *p++);
                break;
            }

            if (orig_mon != cur_mon) {
              pos_x = 0;
              align = ALIGN_L;
            }
          } break;

          // Draw a N-pixel wide empty character.
          case 'O': {
            errno = 0;
            int w = int(strtoul(p, &p, 10));
            if (errno)
              continue;

            draw_shift(*cur_mon, pos_x, align, w);

            pos_x += w;
            area_shift(cur_mon->window, align, w);
          } break;

          case 'T': {
              if (*p == '-') {
                // Switch to automatic font selection.
                font_index = -1;
                p++;
              } else if (isdigit(*p)) {
                font_index = int(strtoul(p, &ep, 10));
                // User-specified 'font_index' ∊ (0,font_count]
                // Otherwise just fallback to the automatic font selection
                if (!font_index || font_index > font_list.size()) {
                  fprintf(stderr, "Invalid font index %d\n", font_index);
                  font_index = -1;
                }
                p = ep;
              } else {
                // Swallow the invalid character and keep parsing.
                fprintf(stderr, "Invalid font slot \"%c\"\n", *p++);
              }
          } break;

          // In case of error keep parsing after the closing }
          default:
            p = block_end;
        }
      }
      // Eat the trailing }
      p++;
    } else { // utf-8 -> ucs-2
      // Escaped % symbol, eat the first one
      if (p[0] == '%' && p[1] == '%')
        p++;

      uint8_t *utf = (uint8_t *)p;
      uint16_t ucs;

      // ASCII
      if (utf[0] < 0x80) {
        ucs = utf[0];
        p  += 1;
      }
      // Two byte utf8 sequence
      else if ((utf[0] & 0xe0) == 0xc0) {
        ucs = (utf[0] & 0x1f) << 6 | (utf[1] & 0x3f);
        p += 2;
      }
      // Three byte utf8 sequence
      else if ((utf[0] & 0xf0) == 0xe0) {
        ucs = (utf[0] & 0xf) << 12 | (utf[1] & 0x3f) << 6 | (utf[2] & 0x3f);
        p += 3;
      }
      // Four byte utf8 sequence
      else if ((utf[0] & 0xf8) == 0xf0) {
        ucs = 0xfffd;
        p += 4;
      }
      // Five byte utf8 sequence
      else if ((utf[0] & 0xfc) == 0xf8) {
        ucs = 0xfffd;
        p += 5;
      }
      // Six byte utf8 sequence
      else if ((utf[0] & 0xfe) == 0xfc) {
        ucs = 0xfffd;
        p += 6;
      }
      // Not a valid utf-8 sequence
      else {
        ucs = utf[0];
        p += 1;
      }

      auto cur_font = select_drawable_font(ucs);
      if (!cur_font)
        continue;

      xcb_change_gc(c, gc[GC_DRAW] , XCB_GC_FONT, &cur_font->ptr);

      int w = draw_char(*cur_mon, *cur_font, pos_x, align, ucs);

      pos_x += w;
      area_shift(cur_mon->window, align, w);
    }
  }
}

void
font_load (const char *pattern)
{
  xcb_query_font_cookie_t queryreq;
  xcb_query_font_reply_t *font_info;
  xcb_void_cookie_t cookie;
  xcb_font_t font;

  font = xcb_generate_id(c);

  cookie = xcb_open_font_checked(c, font, strlen(pattern), pattern);
  if (xcb_request_check (c, cookie)) {
    fprintf(stderr, "Could not load font \"%s\"\n", pattern);
    return;
  }

  auto ret = std::make_unique<font_t>();

  queryreq = xcb_query_font(c, font);
  font_info = xcb_query_font_reply(c, queryreq, nullptr);

  ret->ptr = font;
  ret->descent = font_info->font_descent;
  ret->height = font_info->font_ascent + font_info->font_descent;
  ret->width = font_info->max_bounds.character_width;
  ret->char_max = font_info->max_byte1 << 8 | font_info->max_char_or_byte2;
  ret->char_min = font_info->min_byte1 << 8 | font_info->min_char_or_byte2;

  // Copy over the width lut as it's part of font_info
  int lut_size = xcb_query_font_char_infos_length(font_info);
  if (lut_size) {
    auto info_ptr = xcb_query_font_char_infos(font_info);
    ret->width_lut = std::vector<xcb_charinfo_t>(info_ptr, info_ptr + lut_size);
  }

  free(font_info);

  font_list.emplace_back(move(ret));
}

enum {
  NET_WM_WINDOW_TYPE,
  NET_WM_WINDOW_TYPE_DOCK,
  NET_WM_DESKTOP,
  NET_WM_STRUT_PARTIAL,
  NET_WM_STRUT,
  NET_WM_STATE,
  NET_WM_STATE_STICKY,
  NET_WM_STATE_ABOVE,
};

void
set_ewmh_atoms ()
{
  const char *atom_names[] = {
    "_NET_WM_WINDOW_TYPE",
    "_NET_WM_WINDOW_TYPE_DOCK",
    "_NET_WM_DESKTOP",
    "_NET_WM_STRUT_PARTIAL",
    "_NET_WM_STRUT",
    "_NET_WM_STATE",
    // Leave those at the end since are batch-set
    "_NET_WM_STATE_STICKY",
    "_NET_WM_STATE_ABOVE",
  };
  const int atoms = sizeof(atom_names)/sizeof(char *);
  xcb_intern_atom_cookie_t atom_cookie[atoms];
  xcb_atom_t atom_list[atoms];
  xcb_intern_atom_reply_t *atom_reply;

  // As suggested fetch all the cookies first (yum!) and then retrieve the
  // atoms to exploit the async'ness
  for (int i = 0; i < atoms; i++)
    atom_cookie[i] = xcb_intern_atom(c, 0, strlen(atom_names[i]), atom_names[i]);

  for (int i = 0; i < atoms; i++) {
    atom_reply = xcb_intern_atom_reply(c, atom_cookie[i], nullptr);
    if (!atom_reply)
      return;
    atom_list[i] = atom_reply->atom;
    free(atom_reply);
  }

  // Prepare the strut array
  for (auto& mon : mon_list) {
    int strut[12] = {0};
    if (topbar) {
      strut[2] = bh;
      strut[8] = mon.x;
      strut[9] = mon.x + mon.width - 1;
    } else {
      strut[3]  = bh;
      strut[10] = mon.x;
      strut[11] = mon.x + mon.width - 1;
    }
    constexpr unsigned int minus_one = -1;

    xcb_change_property(c, XCB_PROP_MODE_REPLACE, mon.window, atom_list[NET_WM_WINDOW_TYPE], XCB_ATOM_ATOM, 32, 1, &atom_list[NET_WM_WINDOW_TYPE_DOCK]);
    xcb_change_property(c, XCB_PROP_MODE_APPEND,  mon.window, atom_list[NET_WM_STATE], XCB_ATOM_ATOM, 32, 2, &atom_list[NET_WM_STATE_STICKY]);
    xcb_change_property(c, XCB_PROP_MODE_REPLACE, mon.window, atom_list[NET_WM_DESKTOP], XCB_ATOM_CARDINAL, 32, 1, &minus_one );
    xcb_change_property(c, XCB_PROP_MODE_REPLACE, mon.window, atom_list[NET_WM_STRUT_PARTIAL], XCB_ATOM_CARDINAL, 32, 12, strut);
    xcb_change_property(c, XCB_PROP_MODE_REPLACE, mon.window, atom_list[NET_WM_STRUT], XCB_ATOM_CARDINAL, 32, 4, strut);
    xcb_change_property(c, XCB_PROP_MODE_REPLACE, mon.window, XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 8, 3, "bar");
  }
}

monitor_t
monitor_new (int x, int y, int width, int height, std::string name)
{
  y = (topbar ? by : height - bh - by) + y;
  auto window = xcb_generate_id(c);

  int depth = (visual == scr->root_visual) ? XCB_COPY_FROM_PARENT : 32;
  uint32_t window_values[] = { bgc.v, bgc.v, dock, XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_BUTTON_PRESS, colormap };
  xcb_create_window(c, depth, window, scr->root,
      x, y, width, bh, 0,
      XCB_WINDOW_CLASS_INPUT_OUTPUT, visual,
      XCB_CW_BACK_PIXEL | XCB_CW_BORDER_PIXEL | XCB_CW_OVERRIDE_REDIRECT | XCB_CW_EVENT_MASK | XCB_CW_COLORMAP,
      &window_values);

  auto pixmap = xcb_generate_id(c);
  xcb_create_pixmap(c, depth, pixmap, window, width, bh);

  return monitor_t(name, x, y, width, height, window, pixmap);
}

int
mon_sort_cb (const void *p1, const void *p2)
{
  const monitor_t *m1 = (monitor_t *)p1;
  const monitor_t *m2 = (monitor_t *)p2;

  if (m1->x < m2->x || m1->y + m1->height <= m2->y)
    return -1;
  if (m1->x > m2->x || m1->y + m1->height > m2->y)
    return  1;

  return 0;
}

void
monitor_create_chain (monitor_t *mons, const int num)
{
  int i;
  int width = 0, height = 0;
  int left = bx;

  // Sort before use
  qsort(mons, num, sizeof(monitor_t), mon_sort_cb);

  for (i = 0; i < num; i++) {
    int h = mons[i].y + mons[i].height;
    // Accumulated width of all monitors
    width += mons[i].width;
    // Get height of screen from y_offset + height of lowest monitor
    if (h >= height)
    height = h;
  }

  if (bw < 0)
    bw = width - bx;

  // Use the first font height as all the font heights have been set to the biggest of the set
  if (bh < 0 || bh > height)
    bh = font_list[0]->height + bu + 2;

  // Check the geometry
  if (bx + bw > width || by + bh > height) {
    fprintf(stderr, "The geometry specified doesn't fit the screen!\n");
    exit(EXIT_FAILURE);
  }

  // Left is a positive number or zero therefore monitors with zero width are excluded
  width = bw;
  for (i = 0; i < num; i++) {
    if (mons[i].y + mons[i].height < by)
      continue;
    if (mons[i].width > left) {
      mon_list.emplace_back(monitor_new(
          mons[i].x + left,
          mons[i].y,
          min(width, mons[i].width - left),
          mons[i].height,
          mons[i].name
      ));

      width -= mons[i].width - left;

      // No need to check for other monitors
      if (width <= 0)
        break;
    }

    left -= mons[i].width;

    if (left < 0)
      left = 0;
  }
}

void
get_randr_monitors ()
{
  xcb_randr_get_screen_resources_current_reply_t *rres_reply;
  xcb_randr_output_t *outputs;
  int i, j, num, valid = 0;

  rres_reply = xcb_randr_get_screen_resources_current_reply(c,
      xcb_randr_get_screen_resources_current(c, scr->root), nullptr);

  if (!rres_reply) {
    fprintf(stderr, "Failed to get current randr screen resources\n");
    return;
  }

  num = xcb_randr_get_screen_resources_current_outputs_length(rres_reply);
  outputs = xcb_randr_get_screen_resources_current_outputs(rres_reply);

  // There should be at least one output
  if (num < 1) {
    free(rres_reply);
    return;
  }

  // Every entry starts with a size of 0, making it invalid until we fill in
  // the data retrieved from the Xserver.
  std::vector<monitor_t> mons;

  // Get all outputs
  for (i = 0; i < num; i++) {
    xcb_randr_get_output_info_reply_t *oi_reply;
    xcb_randr_get_crtc_info_reply_t *ci_reply;

    oi_reply = xcb_randr_get_output_info_reply(c, xcb_randr_get_output_info(c, outputs[i], XCB_CURRENT_TIME), nullptr);

    // Output disconnected or not attached to any CRTC ?
    if (!oi_reply || oi_reply->crtc == XCB_NONE || oi_reply->connection != XCB_RANDR_CONNECTION_CONNECTED) {
      free(oi_reply);
      continue;
    }

    ci_reply = xcb_randr_get_crtc_info_reply(c,
        xcb_randr_get_crtc_info(c, oi_reply->crtc, XCB_CURRENT_TIME), nullptr);

    if (!ci_reply) {
      fprintf(stderr, "Failed to get RandR crtc info\n");
      free(rres_reply);
      return;
    }

    int name_len = xcb_randr_get_output_info_name_length(oi_reply);
    std::string name_ptr(reinterpret_cast<char*>(xcb_randr_get_output_info_name(oi_reply)));

    bool is_valid = true;

    if (is_valid) {
      // There's no need to handle rotated screens here (see #69)
      mons.emplace_back(name_ptr, uint32_t(ci_reply->x), uint32_t(ci_reply->y),
        ci_reply->width, ci_reply->height, 0, 0);
      valid += 1;
    }

    free(oi_reply);
    free(ci_reply);
  }

  free(rres_reply);

  // Check for clones and inactive outputs
  for (i = 0; i < num; i++) {
    if (mons[i].width == 0)
      continue;

    for (j = 0; j < num; j++) {
      // Does I contain J ?

      if (i != j && mons[j].width && mons[j].name.empty()) {
        if (mons[j].x >= mons[i].x && mons[j].x + mons[j].width <= mons[i].x + mons[i].width &&
          mons[j].y >= mons[i].y && mons[j].y + mons[j].height <= mons[i].y + mons[i].height) {
          mons[j].width = 0;
          valid--;
        }
      }
    }
  }

  if (valid > 0) {
    monitor_t valid_mons[valid];
    for (i = j = 0; i < num && j < valid; i++) {
      if (mons[i].width != 0) {
        valid_mons[j++] = mons[i];
      }
    }

    monitor_create_chain(valid_mons, valid);
  } else {
    fprintf(stderr, "No usable RandR output found\n");
  }
}

#ifdef WITH_XINERAMA
void
get_xinerama_monitors ()
{
  xcb_xinerama_query_screens_reply_t *xqs_reply;
  xcb_xinerama_screen_info_iterator_t iter;
  int screens;

  xqs_reply = xcb_xinerama_query_screens_reply(c,
      xcb_xinerama_query_screens_unchecked(c), nullptr);

  iter = xcb_xinerama_query_screens_screen_info_iterator(xqs_reply);
  screens = iter.rem;

  monitor_t mons[screens];

  // Fetch all the screens first
  for (int i = 0; iter.rem; i++) {
    mons[i].name = nullptr;
    mons[i].x = iter.data->x_org;
    mons[i].y = iter.data->y_org;
    mons[i].width = iter.data->width;
    mons[i].height = iter.data->height;
    xcb_xinerama_screen_info_next(&iter);
  }

  free(xqs_reply);

  monitor_create_chain(mons, screens);
}
#endif

xcb_visualid_t
get_visual ()
{
  xcb_depth_iterator_t iter;

  iter = xcb_screen_allowed_depths_iterator(scr);

  // Try to find a RGBA visual
  while (iter.rem) {
    xcb_visualtype_t *vis = xcb_depth_visuals(iter.data);

    if (iter.data->depth == 32)
      return vis->visual_id;

    xcb_depth_next(&iter);
  }

  // Fallback to the default one
  return scr->root_visual;
}

// Parse an X-styled geometry string, we don't support signed offsets though.
bool
parse_geometry_string (char *str, int *tmp)
{
  char *p = str;
  int i = 0, j;

  if (!str || !str[0])
    return false;

  // The leading = is optional
  if (*p == '=')
    p++;

  while (*p) {
    // A geometry string has only 4 fields
    if (i >= 4) {
      fprintf(stderr, "Invalid geometry specified\n");
      return false;
    }
    // Move on if we encounter a 'x' or '+'
    if (*p == 'x') {
      if (i > 0) // The 'x' must precede '+'
        break;
      i++; p++; continue;
    }
    if (*p == '+') {
      if (i < 1) // Stray '+', skip the first two fields
        i = 2;
      else
        i++;
      p++; continue;
    }
    // A digit must follow
    if (!isdigit(*p)) {
      fprintf(stderr, "Invalid geometry specified\n");
      return false;
    }
    // Try to parse the number
    errno = 0;
    j = strtoul(p, &p, 10);
    if (errno) {
      fprintf(stderr, "Invalid geometry specified\n");
      return false;
    }
    tmp[i] = j;
  }

  return true;
}

void
parse_output_string(char *str)
{
  if (!str || !*str)
    return;
  output_names.emplace_back(str);
}

void
xconn ()
{
  // Connect to X
  c = xcb_connect (nullptr, nullptr);
  if (xcb_connection_has_error(c)) {
    fprintf(stderr, "Couldn't connect to X\n");
    exit(EXIT_FAILURE);
  }

  // Grab infos from the first screen
  scr = xcb_setup_roots_iterator(xcb_get_setup(c)).data;

  // Try to get a RGBA visual and build the colormap for that
  visual = get_visual();

  colormap = xcb_generate_id(c);
  xcb_create_colormap(c, XCB_COLORMAP_ALLOC_NONE, colormap, scr->root, visual);
}

void
init (char *wm_name)
{
  // Try to load a default font
  if (font_list.size() == 0)
    font_load("fixed");

  // We tried and failed hard, there's something wrong
  if (!font_list.size())
    exit(EXIT_FAILURE);

  // To make the alignment uniform, find maximum height
  int maxh = font_list[0]->height;
  for (int i = 1; i < font_list.size(); i++)
    maxh = max(maxh, font_list[i]->height);

  // Set maximum height to all fonts
  for (int i = 0; i < font_list.size(); i++)
    font_list[i]->height = maxh;

  // Generate a list of screens
  const xcb_query_extension_reply_t *qe_reply;

  // Initialize monitor list head and tail
  mon_list.clear();

  // Check if RandR is present
  qe_reply = xcb_get_extension_data(c, &xcb_randr_id);

  if (qe_reply && qe_reply->present) {
    get_randr_monitors();
  }
#if WITH_XINERAMA
  else {
    qe_reply = xcb_get_extension_data(c, &xcb_xinerama_id);

    // Check if Xinerama extension is present and active
    if (qe_reply && qe_reply->present) {
      xcb_xinerama_is_active_reply_t *xia_reply;
      xia_reply = xcb_xinerama_is_active_reply(c, xcb_xinerama_is_active(c), nullptr);

      if (xia_reply && xia_reply->state)
        get_xinerama_monitors();

      free(xia_reply);
    }
  }
#endif

  if (mon_list.empty()) {
    // If I fits I sits
    if (bw < 0)
      bw = scr->width_in_pixels - bx;

    // Adjust the height
    if (bh < 0 || bh > scr->height_in_pixels)
      bh = maxh + bu + 2;

    // Check the geometry
    if (bx + bw > scr->width_in_pixels || by + bh > scr->height_in_pixels) {
      fprintf(stderr, "The geometry specified doesn't fit the screen!\n");
      exit(EXIT_FAILURE);
    }

    // If no RandR outputs or Xinerama screens, fall back to using whole screen
    mon_list.emplace_back(monitor_new(0, 0, bw, scr->height_in_pixels, ""));
  }

  if (mon_list.empty())
    exit(EXIT_FAILURE);

  // For WM that support EWMH atoms
  set_ewmh_atoms();

  // Create the gc for drawing
  gc[GC_DRAW] = xcb_generate_id(c);
  xcb_create_gc(c, gc[GC_DRAW], mon_list.front().pixmap, XCB_GC_FOREGROUND, &fgc.v);

  gc[GC_CLEAR] = xcb_generate_id(c);
  xcb_create_gc(c, gc[GC_CLEAR], mon_list.front().pixmap, XCB_GC_FOREGROUND, &bgc.v);

  gc[GC_ATTR] = xcb_generate_id(c);
  xcb_create_gc(c, gc[GC_ATTR], mon_list.front().pixmap, XCB_GC_FOREGROUND, &ugc.v);

  // Make the bar visible and clear the pixmap
  for (auto& mon : mon_list) {
    fill_rect(mon.pixmap, gc[GC_CLEAR], 0, 0, mon.width, bh);
    xcb_map_window(c, mon.window);

    uint32_t tmp[] = {mon.x, mon.y};
    // Make sure that the window really gets in the place it's supposed to be
    // Some WM such as Openbox need this
    xcb_configure_window(c, mon.window, XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y, &tmp);

    // Set the WM_NAME atom to the user specified value
    if (wm_name)
      xcb_change_property(c, XCB_PROP_MODE_REPLACE, mon.window, XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 8 ,strlen(wm_name), wm_name);
  }

  xcb_flush(c);
}

void
cleanup ()
{
  for (int i = 0; i < font_list.size(); i++) {
    xcb_close_font(c, font_list[i]->ptr);
  }

  for(auto& mon : mon_list) {
    xcb_destroy_window(c, mon.window);
    xcb_free_pixmap(c, mon.pixmap);
  }

  xcb_free_colormap(c, colormap);

  if (gc[GC_DRAW])
    xcb_free_gc(c, gc[GC_DRAW]);
  if (gc[GC_CLEAR])
    xcb_free_gc(c, gc[GC_CLEAR]);
  if (gc[GC_ATTR])
    xcb_free_gc(c, gc[GC_ATTR]);
  if (c)
    xcb_disconnect(c);
}

void
sighandle (int signal)
{
  if (signal == SIGINT || signal == SIGTERM)
    exit(EXIT_SUCCESS);
}

int
main (int argc, char **argv)
{
  struct pollfd pollin[2] = {
    { .fd = STDIN_FILENO, .events = POLLIN },
    { .fd = -1      , .events = POLLIN },
  };
  xcb_generic_event_t *ev;
  xcb_expose_event_t *expose_ev;
  xcb_button_press_event_t *press_ev;
  char input[256];
  size_t input_offset = 0;
  bool permanent = false;
  int geom_v[4] = { -1, -1, 0, 0 };
  int ch;
  char *wm_name;

  // Install the parachute!
  atexit(cleanup);
  signal(SIGINT, sighandle);
  signal(SIGTERM, sighandle);

  // B/W combo
  dbgc = bgc = BLACK;
  dfgc = fgc = WHITE;
  dugc = ugc = fgc;

  // A safe default
  wm_name = nullptr;

  // Connect to the Xserver and initialize scr
  xconn();

  while ((ch = getopt(argc, argv, "hg:o:bdf:a:pu:B:F:U:n:")) != -1) {
    switch (ch) {
      case 'h':
        printf ("lemonbar version %s\n", "x.x");
        printf ("usage: %s [-h | -g | -o | -b | -d | -f | -p | -n | -u | -B | -F]\n"
            "\t-h Show this help\n"
            "\t-g Set the bar geometry {width}x{height}+{xoffset}+{yoffset}\n"
            "\t-o Add randr output by name\n"
            "\t-b Put the bar at the bottom of the screen\n"
            "\t-d Force docking (use this if your WM isn't EWMH compliant)\n"
            "\t-f Set the font name to use\n"
            "\t-p Don't close after the data ends\n"
            "\t-n Set the WM_NAME atom to the specified value for this bar\n"
            "\t-u Set the underline/overline height in pixels\n"
            "\t-B Set background color in #AARRGGBB\n"
            "\t-F Set foreground color in #AARRGGBB\n", argv[0]);
        exit (EXIT_SUCCESS);
      case 'g': parse_geometry_string(optarg, geom_v); break;
      case 'o': parse_output_string(optarg); break;
      case 'p': permanent = true; break;
      case 'n': wm_name = strdup(optarg); break;
      case 'b': topbar = false; break;
      case 'd': dock = true; break;
      case 'f': font_load(optarg); break;
      case 'u': bu = strtoul(optarg, nullptr, 10); break;
      case 'B': dbgc = bgc = parse_color(optarg, nullptr, BLACK); break;
      case 'F': dfgc = fgc = parse_color(optarg, nullptr, WHITE); break;
      case 'U': dugc = ugc = parse_color(optarg, nullptr, fgc); break;
    }
  }

  // Copy the geometry values in place
  bw = geom_v[0];
  bh = geom_v[1];
  bx = geom_v[2];
  by = geom_v[3];

  // Do the heavy lifting
  init(wm_name);
  // The string is strdup'd when the command line arguments are parsed
  free(wm_name);
  // Get the fd to Xserver
  pollin[1].fd = xcb_get_file_descriptor(c);

  for (;;) {
    bool redraw = false;

    // If connection is in error state, then it has been shut down.
    if (xcb_connection_has_error(c))
      break;

    if (poll(pollin, 2, -1) > 0) {
      if (pollin[0].revents & POLLHUP) {    // No more data...
        if (permanent) pollin[0].fd = -1;   // ...null the fd and continue polling :D
        else break;             // ...bail out
      }
      if (pollin[0].revents & POLLIN) { // New input, process it
        while (true) {
          ssize_t r = read(STDIN_FILENO, input + input_offset,
              sizeof(input) - input_offset);
          if (r == 0) break;
          if (r < 0) {
            if (errno == EINTR) continue;
            exit(EXIT_FAILURE);
          }

          input_offset += r;

          // Try to find the last complete input line in the buffer.
          char *input_end = input + input_offset;
          char *last_nl = _memrchr(input, '\n', input_end - input);

          if (last_nl) {
            char *prev_nl = (last_nl != input) ?
                _memrchr(input, '\n', last_nl - 1 - input) : nullptr;
            char *begin = prev_nl? prev_nl + 1: input;

            *last_nl = '\0';

            parse(begin);
            redraw = true;

            // Move the unparsed part back to the beginning.
            const size_t remaining = input_end - (last_nl + 1);
            if (remaining != 0) memmove(input, last_nl + 1, remaining);
            input_offset = remaining;

            break;
          }

          // The input buffer is full and we haven't seen a newline
          // yet, discard everything and start from zero.
          if (sizeof(input) == input_offset) {
            input_offset = 0;
          }
        }
      }
      if (pollin[1].revents & POLLIN) { // The event comes from the Xorg server
        while ((ev = xcb_poll_for_event(c))) {
          expose_ev = (xcb_expose_event_t *)ev;

          switch (ev->response_type & 0x7F) {
            case XCB_EXPOSE:
              if (expose_ev->count == 0)
                redraw = true;
              break;
            case XCB_BUTTON_PRESS:
              press_ev = (xcb_button_press_event_t *)ev;
              {
                area_t *area = area_get(press_ev->event, press_ev->detail, press_ev->event_x);
                // Respond to the click
                if (area) {
                  write(STDOUT_FILENO, area->cmd, strlen(area->cmd));
                  write(STDOUT_FILENO, "\n", 1);
                }
              }
              break;
          }

          free(ev);
        }
      }
    }

    if (redraw) { // Copy our temporary pixmap onto the window
      for (auto& mon : mon_list) {
        xcb_copy_area(c, mon.pixmap, mon.window, gc[GC_DRAW], 0, 0, 0, 0, mon.width, bh);
      }
    }

    xcb_flush(c);
  }

  return EXIT_SUCCESS;
}

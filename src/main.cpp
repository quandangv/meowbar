#include "common.hpp"

#include <lini/parse.hpp>
#include <fstream>
#include <iostream>
#include <unistd.h>
#include <xcb/xcb.h>

using std::cout;

void show_help() {
  cout << "arg1: the name of document file." << endl;
  cout << "arg2: the key that store the output." << endl;
}

int main(int argc, char** argv) {
  if(argc < 3) {
    show_help();
    return 0;
  }
  std::ifstream ifs{argv[1]};
  lini::document doc;
  lini::errorlist err;
  lini::parse(ifs, doc, err);
  ifs.close();
  for(auto e : err)
    LG_ERR("Error in document at " << e.first + ": " << e.second);

  auto output = doc.get_child(argv[2]);
  if (!output) {
    LG_ERR("Can't retrieve the output key: " << argv[2])
    return 2;
  }
}

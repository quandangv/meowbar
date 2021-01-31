cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

project(meowbar VERSION 1.1.0)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR})
include(file_list)
include(FindX11)
if(NOT (X11_FOUND AND X11_Xrandr_FOUND AND X11_Xext_FOUND))
  msg(FATAL_ERROR "Can't find X11")
endif()
find_library(lini lini REQUIRED)

#include(cmake/targets)
add_executable(meowbar_exec ${EXECUTABLES})
target_include_directories(meowbar_exec PUBLIC ${INCLUDE_DIRS}
  ${X11_INCLUDE_DIR}
  ${X11_Xinerama_INCLUDE_PATH}
)
message("Libs: ${X11_LIBRARIES}")
target_link_libraries(meowbar_exec ${lini}
  /usr/lib/libxcb.so
  /usr/lib/libxcb-xinerama.so
  /usr/lib/libxcb-randr.so
)

configure_file(${PRIVATE_HEADERS_DIR}/common.hpp.in  ${GENERATED_HEADERS_DIR}/common.hpp)

if(BUILD_TESTS)
  include(cmake/test)
endif()

unset(DEBUG_SCOPES CACHE)


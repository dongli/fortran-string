cmake_minimum_required(VERSION 2.8)

project(fortran-string LANGUAGES Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
  if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan")
  else ()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffpe-summary=none -Ofast")
  endif ()
endif ()

set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}")
include_directories(${CMAKE_BINARY_DIR})

get_directory_property(parent_dir PARENT_DIRECTORY)
if (EXISTS ${PROJECT_SOURCE_DIR}/lib/unit-test/CMakeLists.txt AND NOT parent_dir)
  set(HAS_UNIT_TEST ON)
  add_subdirectory(lib/unit-test)
endif ()

set(sources
  src/string_mod.F90
  src/string_actions_mod.F90
  src/string_numerics_mod.F90
  src/string.F90
)

add_library(fortran_string ${sources})

if (HAS_UNIT_TEST)
  add_executable(string_test.exe src/string_test.F90)
  target_link_libraries(string_test.exe fortran_string fortran_unit_test)
endif ()

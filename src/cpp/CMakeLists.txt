cmake_minimum_required (VERSION 2.6)

project(qtRendering)

# set(CMAKE_VERBOSE_MAKEFILE 1)
set(CMAKE_BUILD_TYPE "RELEASE") # -O3

message(build-type: ${CMAKE_BUILD_TYPE})

find_package(Qt4 REQUIRED)

set(QT_USE_QTOPENGL TRUE)
include(${QT_USE_FILE})

set(HEADERS qtwrapper.h)
qt4_wrap_cpp(MOCFILES ${HEADERS})
add_library(qtwrapper qtwrapper.cpp ${MOCFILES})

# added for ghci debugging
IF(UNIX)
    add_library(qtwrapper_so SHARED qtwrapper.cpp ${MOCFILES})
ENDIF(UNIX)

add_custom_command(TARGET qtwrapper POST_BUILD
                   COMMAND touch
                   ../../Main.hs) # to force re-linking


cmake_minimum_required (VERSION 2.6)

project(qtRendering)

# set(CMAKE_VERBOSE_MAKEFILE 1)
set(CMAKE_BUILD_TYPE "RELEASE") # -O3
message(STATUS "build-type: " ${CMAKE_BUILD_TYPE})

set(ARCH i386)
set(CMAKE_OSX_ARCHITECTURES i386)

find_package(Qt4 REQUIRED)

set(QT_USE_QTOPENGL TRUE)
include(${QT_USE_FILE})

set(CLASS_HEADERS MainWindow.h GLContext.h FallbackContext.h)
qt4_wrap_cpp(MOCFILES ${CLASS_HEADERS})
set(SRCS utils.cpp MainWindow.cpp GLContext.cpp FallbackContext.cpp qtwrapper.cpp ${MOCFILES})
add_library(qtwrapper ${SRCS})

# added for ghci debugging
IF("${CMAKE_SYSTEM}" MATCHES "Linux")
    add_library(qtwrapper_so SHARED ${SRCS})
ENDIF("${CMAKE_SYSTEM}" MATCHES "Linux")

add_custom_command(TARGET qtwrapper POST_BUILD
                   COMMAND touch
                   ../../Main.hs) # to force re-linking

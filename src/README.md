# Installing and Running Nikki

This directory contains all source files for Nikki and the Robots. 

## Setup On Linux

These directions are going to be tailored for a Debian-based Linux distro, but
they should be adaptable to any Linux distro. The rationale for each step is
laid out so hopefully it can be easily ported to other platforms (e.g. a BSD).

_If you're on a Debian-based system and you don't want to read the rest, do the
following, then skip directly to the section "Running Nikki"_:

``` bash
$ sudo apt-get install haskell-platform g++ cmake pkg-config libzip-dev libopenal-dev libsndfile1-dev
$ ./linuxCompile.sh --extra-include-dir=/usr/include/AL
```
If the above fails out with a message about `libzip`, this is probably because
you have a version of `libzip-dev` installed which is different from the version
of the Haskell library `libzip` which gets pulled down. Re-run `linuxCompile.sh`
with a constraint on the version of `libzip-dev` that is installed, leaving the
patch version free. For example if version 0.10.2 is installed, then run the
following:

``` bash
$ ./linuxCompile.sh --extra-include-dir=/usr/include/AL --constraint=libzip==0.10.*
```

First make sure you have the actual build tools necessary to build Nikki. 
+ Since Nikki is built with GHC Haskell, you will need GHC and `cabal`. The
  easiest way to do this on Debian is `sudo apt-get install haskell-platform`.
+ You will need a working C compiler for the Foreign Function Interface for
  Haskell to hook into many of the sound and graphics libraries. Linux distros
  always ship with GCC, so you should be fine here.
+ You will need a working C++ compiler for the Qt components. g++ will do nicely
  here. For Debian: `sudo apt-get install g++`.
+ You will need Make for building the Qt components. Linux distros always ship
  with Make so you're fine here.
+ You will need CMake, also for building the Qt components. For Debian: `sudo
  apt-get install cmake`.
+ You will need Qt4 itself for Nikki's GUI. For Debian: `sudo apt-get install
  libqt4-dev`.

Nikki has a few native dependencies that will have to be installed first that
are required for a lot of its Haskell dependencies to compile.

+ Nikki depends on the C library `libzip` via the Haskell library `libzip` which
  in turn (transitively through the Haskell package `bindings-libzip`) depends
  on the actual C `libzip` library. Therefore you must have the `libzip` header
  files. For Debian: `sudo apt-get install libzip-dev`
+ Two Haskell dependencies use `pkg-config`. `libzip`'s transitive dependency
  `bindings-libzip` uses it by default although it can be turned off by the
  `fNoPkgConfig` Cabal flag. `clocked` requires it. For Debian: `sudo apt-get
  install pkg-config`.
+ The Haskell library `sfml-audio` requires OpenAL and libsndfile.
  Frustratingly, probably as a result of SFML's own [quirkiness][1], even if you
  install OpenAL using your distro's package manager, `sfml-audio` probably
  won't find the `al.h` file it needs. You will probably need to manually pass
  in its location to `linuxCompile.sh` (see the later steps for more on this).
  For Debian: `sudo apt-get install libopenal-dev libsndfile1-dev`.

Once you have these dependencies installed, compile on Linux with where
`LOCATION_OF_AL_HEADER` is the path to the folder containing the file `al.h` (as
noted earlier `sfml` often looks for OpenAL in a non-standard place, usually
missing the `AL` directory):

``` bash
$ ./linuxCompile.sh --extra-include-dir=LOCATION_OF_AL_HEADER
```

For example on most Debian-based distros, this is
``` bash
$ ./linuxCompile.sh --extra-include-dir=/usr/include/AL
```

### Reference Cabal Freeze File

For reference, there is a `cabal.config` file generated with `cabal freeze` for
a particular GHC 7.8.3 build. It is currently suffixed with `.inactive` so that
`cabal` doesn't use it by default since Nikki should build across several
different GHC versions.

## Running Nikki

Run the game with:

``` bash
$ ./linuxRunInPlace.sh
```

## Feedback

If you have any questions or comments regarding compiling or installing, please
open issues here:

https://github.com/nikki-and-the-robots/nikki/issues

or send me a mail:

shahn@joyridelabs.de

[1]: http://en.sfml-dev.org/forums/index.php?topic=3056.0

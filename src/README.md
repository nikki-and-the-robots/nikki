# Installing and Running Nikki

This directory contains all source files for Nikki and the Robots.

## Setup On Linux

These directions are going to be tailored for a Debian-based Linux distro, but
they should be adaptable to any Linux distro. The rationale for each step is
laid out so hopefully it can be easily ported to other platforms (e.g. a BSD).

_If you're on a Debian-based system and you don't want to read the rest, do the
following, then skip directly to the section "Running Nikki"_:

```bash
$ sudo apt-get install g++ cmake pkg-config libzip-dev libopenal-dev libsndfile1-dev libqt4-dev
$ ./build-qtwrapper.sh
$ stack setup
$ stack build
```

Nikki has a few native dependencies that will have to be installed first that
are required for a lot of its Haskell dependencies to compile.

- Nikki depends on the C library `libzip` via the Haskell library `libzip` which
  in turn (transitively through the Haskell package `bindings-libzip`) depends
  on the actual C `libzip` library. Therefore you must have the `libzip` header
  files. For Debian: `sudo apt-get install libzip-dev`
- Two Haskell dependencies use `pkg-config`. `libzip`'s transitive dependency
  `bindings-libzip` uses it by default although it can be turned off by the
  `fNoPkgConfig` Cabal flag. `clocked` requires it. For Debian:
  `sudo apt-get install pkg-config`.

## Running Nikki

Run the game with:

```bash
$ ./linuxRunInPlace.sh
```

## Feedback

If you have any questions or comments regarding compiling or installing, please
open issues here:

https://github.com/nikki-and-the-robots/nikki/issues

or send me a mail:

soenkehahn@gmail.com

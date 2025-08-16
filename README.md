# Fortran Demo

This repository contains a collection of small Fortran 2008 programs and modules that demonstrate numerical computing, parallelization, and modular programming techniques.

## Features
- Modular source organization (`src/`)
- Makefile-based build system with debug/release targets
- Examples of numerical routines (linear algebra, FFT, RNG, statistics)
- Parallel constructs with OpenMP
- File I/O helpers
- Console utilities

## Build
make

Build output is placed under `build/`.

## Run
make run

## Clean
make clean

## Requirements
- gfortran (tested with Fortran 2008 support)
- make
- macOS or Linux

## Repository Layout
- src/ – source files
- build/obj/ – object files
- build/mod/ – module interface files
- build/app – compiled binary
- .gitignore – ignores build artifacts and editor junk
- Makefile – build rules

## License
MIT License 

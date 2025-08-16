#==============================================================
# Fortran project Makefile (release + debug, correct module order)
#==============================================================

FC = gfortran

RELEASE_FLAGS = -std=f2008 -Wall -Wextra -O2 -g -fopenmp \
                -Ibuild/mod -J build/mod
DEBUG_FLAGS   = -std=f2008 -Wall -Wextra -O0 -g -fcheck=all -fbacktrace \
                -Ibuild/mod -J build/mod

# Key sources (adjust if you rename)
MAIN     := src/main.f90
KINDS    := src/kinds.f90
UTILS    := src/utils.f90
RNG      := src/rng.f90
LINALG   := src/linalg.f90
PARALLEL := src/parallel.f90

ALLSRC   := $(wildcard src/*.f90)
# Build these early in this order; then everything else; then main
ORDERED_HEAD := $(KINDS) $(UTILS) $(RNG) $(LINALG) $(PARALLEL)
OTHERS       := $(filter-out $(MAIN) $(ORDERED_HEAD),$(ALLSRC))
ORDERED      := $(ORDERED_HEAD) $(OTHERS) $(MAIN)

# Objects
OBJ_RELEASE := $(patsubst src/%.f90,build/obj/release/%.o,$(ORDERED))
OBJ_DEBUG   := $(patsubst src/%.f90,build/obj/debug/%.o,$(ORDERED))

BIN_RELEASE = build/app
BIN_DEBUG   = build/app-debug

#--------------------------------------------------------------
# Default: release build
#--------------------------------------------------------------
.PHONY: all
all: $(BIN_RELEASE)

$(BIN_RELEASE): build/obj/release build/mod $(OBJ_RELEASE)
	$(FC) $(RELEASE_FLAGS) $(OBJ_RELEASE) -o $@

build/obj/release/%.o: src/%.f90 | build/obj/release build/mod
	$(FC) $(RELEASE_FLAGS) -c $< -o $@

#--------------------------------------------------------------
# Debug build
#--------------------------------------------------------------
.PHONY: debug
debug: $(BIN_DEBUG)

$(BIN_DEBUG): build/obj/debug build/mod $(OBJ_DEBUG)
	$(FC) $(DEBUG_FLAGS) $(OBJ_DEBUG) -o $@

build/obj/debug/%.o: src/%.f90 | build/obj/debug build/mod
	$(FC) $(DEBUG_FLAGS) -c $< -o $@

#--------------------------------------------------------------
# Explicit module deps (so .mod producers compile first)
# Release:
build/obj/release/utils.o   : build/obj/release/kinds.o
build/obj/release/fft.o     : build/obj/release/kinds.o
build/obj/release/stats.o   : build/obj/release/kinds.o build/obj/release/utils.o
build/obj/release/shapes.o  : build/obj/release/kinds.o
build/obj/release/parallel.o: build/obj/release/rng.o
build/obj/release/main.o    : build/obj/release/rng.o build/obj/release/linalg.o build/obj/release/parallel.o build/obj/release/utils.o
# Debug:
build/obj/debug/utils.o   : build/obj/debug/kinds.o
build/obj/debug/fft.o     : build/obj/debug/kinds.o
build/obj/debug/stats.o   : build/obj/debug/kinds.o build/obj/debug/utils.o
build/obj/debug/shapes.o  : build/obj/debug/kinds.o
build/obj/debug/parallel.o: build/obj/debug/rng.o
build/obj/debug/main.o    : build/obj/debug/rng.o build/obj/debug/linalg.o build/obj/debug/parallel.o build/obj/debug/utils.o

#--------------------------------------------------------------
# Dirs
#--------------------------------------------------------------
build/obj/release:
	mkdir -p $@
build/obj/debug:
	mkdir -p $@
build/mod:
	mkdir -p $@

#--------------------------------------------------------------
# Utilities
#--------------------------------------------------------------
.PHONY: clean
clean:
	rm -rf build
	rm -f *.mod   # remove any stray root-level .mod files

.PHONY: run
run: $(BIN_RELEASE)
	./$(BIN_RELEASE)

.PHONY: run-debug
run-debug: $(BIN_DEBUG)
	./$(BIN_DEBUG)

.PHONY: rebuild
rebuild:
	$(MAKE) clean
	$(MAKE) all


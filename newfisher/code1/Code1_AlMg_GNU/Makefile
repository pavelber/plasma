# Makefile for Code1 Al+Mg (GNU Fortran, no NAG)
#
# Windows (WinLibs / MinGW-w64):   mingw32-make
# Linux:                           make
#
# Targets:
#   all (default)  - build $(EXE)
#   debug          - build with bounds checking and backtrace
#   run            - build and run in this folder (needs the *.inp files here)
#   clean          - remove objects, .mod files and the executable
#
# NAG replacements: if nag_replacements_AlMg_GNU_v01.f90 (original from the report)
# is present it is used, otherwise nag_replacements_win.f90.

FC      ?= gfortran
ifeq ($(FC),f77)
  FC := gfortran
endif

ifeq ($(OS),Windows_NT)
  EXE     := Code1_AlMg.exe
  # Static link: the .exe runs on any Windows PC without the MinGW DLLs
  LDSTATIC := -static
else
  EXE     := Code1_AlMg
  LDSTATIC :=
endif

NAGSRC  := $(firstword $(wildcard nag_replacements_AlMg_GNU_v01.f90) nag_replacements_win.f90)

# -std=legacy: accept PAUSE and arithmetic IF without warnings
FFLAGS_COMMON := -ffixed-line-length-none -std=legacy
OPT     ?= -O2
FFLAGS  ?= $(OPT) $(FFLAGS_COMMON)
LDFLAGS ?= $(LDSTATIC)

MOD_OBJ  := mo1_AlMg_GNU_v01.o
NAG_OBJ  := $(NAGSRC:.f90=.o)
LZ_OBJ   := lzfix_AlMg_GNU_v01.o
MAIN_OBJ := Code1_AlMg_GNU_v01.o
OBJS     := $(MOD_OBJ) $(NAG_OBJ) $(LZ_OBJ) $(MAIN_OBJ)

.PHONY: all debug run clean

all: $(EXE)

$(EXE): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS)

# module mo1 must be compiled first (produces mo1.mod)
$(MOD_OBJ): mo1_AlMg_GNU_v01.for
	$(FC) $(FFLAGS) -c $< -o $@

$(NAG_OBJ): $(NAGSRC) $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

# output post-processing: .ddd instead of 0.ddd, as in the old MS Fortran build
$(LZ_OBJ): lzfix_AlMg_GNU_v01.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(MAIN_OBJ): Code1_AlMg_GNU_v01.for $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

debug:
	$(MAKE) clean
	$(MAKE) FFLAGS="-O0 -g -fcheck=bounds -fbacktrace $(FFLAGS_COMMON)"

run: $(EXE)
	./$(EXE)

# Works both when make uses cmd.exe and when it uses sh (Git Bash / MSYS / Linux)
ifeq ($(shell echo %OS%),%OS%)
clean:
	rm -f *.o *.mod $(EXE)
else
clean:
	-del /Q *.o *.mod $(EXE) 2>nul
endif

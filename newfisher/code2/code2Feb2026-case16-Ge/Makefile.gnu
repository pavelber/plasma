# Makefile for Code2 (Co2feb26), GNU Fortran, no NAG
#
# Windows (WinLibs / MinGW-w64):   mingw32-make -f Makefile.gnu
# Linux:                           make -f Makefile.gnu
#
# Targets:
#   all (default)  - build $(EXE)
#   debug          - build with bounds checking and backtrace
#   run            - build and run in this folder (needs the *.inp files here)
#   clean          - remove objects, .mod files and the executable
#
# The original Makefile (nmake + MS Fortran PowerStation fl32 + naglib.lib) is kept as is.
#
# Sources:
#   mo1co2feb.for            - module, unchanged
#   Co2feb26_GNU.for         - Co2feb26.for with two tab-started continuation lines
#                              (421, 2438) rewritten as standard column-6 continuations,
#                              and CALL FixLeadingZeros() before the final STOP
#   lzfix_Co2_GNU.f90        - post-processing of the output files: .ddd instead of 0.ddd
#   nag_replacements_Co2_GNU.f90 - D01AHF (adaptive Gauss-Kronrod) and D02EAF
#                              (matrix exponential of the frozen rate matrix PM)

FC      ?= gfortran
ifeq ($(FC),f77)
  FC := gfortran
endif

ifeq ($(OS),Windows_NT)
  EXE     := Co2feb26.exe
  # Static link: the .exe runs on any Windows PC without the MinGW DLLs
  LDSTATIC := -static
else
  EXE     := Co2feb26
  LDSTATIC :=
endif

# Standard 72-column fixed form, as MS Fortran PowerStation: text after column 72 is ignored
# (several statements in Co2feb26.for have comments there without '!').
# -std=legacy: accept PAUSE and other old extensions without warnings
FFLAGS_COMMON := -std=legacy
OPT     ?= -O2
FFLAGS  ?= $(OPT) $(FFLAGS_COMMON)
LDFLAGS ?= $(LDSTATIC)

MOD_OBJ  := mo1co2feb.o
NAG_OBJ  := nag_replacements_Co2_GNU.o
LZ_OBJ   := lzfix_Co2_GNU.o
MAIN_OBJ := Co2feb26_GNU.o
OBJS     := $(MOD_OBJ) $(NAG_OBJ) $(LZ_OBJ) $(MAIN_OBJ)

.PHONY: all debug run clean

all: $(EXE)

$(EXE): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS)

# module mo1co2feb must be compiled first (produces mo1co2feb.mod)
$(MOD_OBJ): mo1co2feb.for
	$(FC) $(FFLAGS) -c $< -o $@

$(NAG_OBJ): nag_replacements_Co2_GNU.f90 $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

# output post-processing: .ddd instead of 0.ddd, as in the old MS Fortran build
$(LZ_OBJ): lzfix_Co2_GNU.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(MAIN_OBJ): Co2feb26_GNU.for $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

debug:
	$(MAKE) -f Makefile.gnu clean
	$(MAKE) -f Makefile.gnu FFLAGS="-O0 -g -fcheck=bounds -fbacktrace $(FFLAGS_COMMON)"

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

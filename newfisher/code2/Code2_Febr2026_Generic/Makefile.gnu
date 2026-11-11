# Makefile for Code2 (Co2feb26), GNU Fortran, no NAG -- Generic version
# (array sizes nQSm, HSSm, MNLe are set at run time from the input files,
#  no recompilation for a new element; see README_Generic.txt)
#
# Windows (WinLibs / MinGW-w64):   mingw32-make -f Makefile.gnu
# Linux:                           make -f Makefile.gnu
#
# Targets:
#   all (default)  - build $(EXE)
#   debug          - build with bounds checking and backtrace
#   run            - build and run in this folder (needs the *.inp files here)
#   clean          - remove objects, .mod files and the executables
#   tests          - build the test programs test_unit, test_step
#   test           - build and run tests T1 + T2 (needs python), ~3 min
#   test-e2e       - run test T3 (3 time steps of the real program, Kr and Ge)
#   test-all       - T1 + T2 + T3
#
# Sources:
#   co2_const.f90, co2_atom.f90, co2_util.f90, mo1co2feb.f90 - modules: constants;
#                              atomic data, rates, channel lists (ALLOCATABLE, sizes at run
#                              time); timers and utilities; plasma state, grids, spectra.
#                              mo1co2feb uses the other three. Element-dependent arrays are ALLOCATABLE,
#                              AllocArrays/AllocLines allocate them at run time
#   Co2feb26_main.f90, Co2feb26_procs.f90 - main program and all procedures of
#                              Co2feb26_GNU.for (split for tests); the GNU port is
#                              Co2feb26.for with two tab-started continuation lines
#                              (421, 2438) rewritten as standard column-6 continuations,
#                              and CALL FixLeadingZeros() before the final STOP;
#                              CALL AllocArrays() at start, line count + AllocLines in LineList
#   lzfix_Co2_GNU.f90        - post-processing of the output files: .ddd instead of 0.ddd
#   nag_replacements_Co2_GNU.f90 - D01AHF (adaptive Gauss-Kronrod) and D02EAF
#                              (matrix exponential of the frozen rate matrix PM), as in Code1

FC      ?= gfortran
ifeq ($(FC),f77)
  FC := gfortran
endif

ifeq ($(OS),Windows_NT)
  EXEEXT  := .exe
  EXE     := Co2feb26_Generic_v3.exe
  # Static link: the .exe runs on any Windows PC without the MinGW DLLs
  LDSTATIC := -static
else
  EXE     := Co2feb26_Generic_v3
  LDSTATIC :=
endif

# Free-form sources (.f90), converted from the 72-column fixed form of MS Fortran
# PowerStation; text that was after column 72 (ignored there) is kept as a comment
# marked '[after column 72, ignored:]'.
# -std=legacy: accept PAUSE and other old extensions without warnings
# OpenMP: parallel loops (number of threads = number of cores, or OMP_NUM_THREADS).
# Results are the same with any number of threads, see tests/run_tests.py --threads.
# Large local arrays that keep values between calls have an explicit SAVE.
OMPFLAGS ?= -fopenmp
FFLAGS_COMMON := -std=legacy $(OMPFLAGS)
OPT     ?= -O2
FFLAGS  ?= $(OPT) $(FFLAGS_COMMON)
LDFLAGS ?= $(LDSTATIC)

CONST_OBJ:= co2_const.o
UTIL_OBJ := co2_util.o
ATOM_OBJ := co2_atom.o
MOD_OBJ  := mo1co2feb.o
MODS_OBJ := $(CONST_OBJ) $(UTIL_OBJ) $(ATOM_OBJ) $(MOD_OBJ)
NAG_OBJ  := nag_replacements_Co2_GNU.o
LZ_OBJ   := lzfix_Co2_GNU.o
PROC_OBJ := Co2feb26_procs.o
MAIN_OBJ := Co2feb26_main.o
LIB_OBJS := $(MODS_OBJ) $(NAG_OBJ) $(LZ_OBJ) $(PROC_OBJ)
OBJS     := $(LIB_OBJS) $(MAIN_OBJ)

.PHONY: all debug run clean tests test test-e2e test-all

all: $(EXE)

$(EXE): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS)

# modules first: co2_const, co2_util, co2_atom, then mo1co2feb (uses the three)
$(CONST_OBJ): co2_const.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(UTIL_OBJ): co2_util.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(ATOM_OBJ): co2_atom.f90 $(CONST_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

$(MOD_OBJ): mo1co2feb.f90 $(CONST_OBJ) $(UTIL_OBJ) $(ATOM_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

$(NAG_OBJ): nag_replacements_Co2_GNU.f90 $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

# output post-processing: .ddd instead of 0.ddd, as in the old MS Fortran build
$(LZ_OBJ): lzfix_Co2_GNU.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(PROC_OBJ): Co2feb26_procs.f90 $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

$(MAIN_OBJ): Co2feb26_main.f90 $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

debug:
	$(MAKE) -f Makefile.gnu clean
	$(MAKE) -f Makefile.gnu FFLAGS="-O0 -g -fcheck=bounds -fbacktrace $(FFLAGS_COMMON)"

# ---- tests (see tests/run_tests.py and PLAN_refactoring_speed.md)
TEST_IO   := tests/testio.o
tests: test_unit$(EXEEXT) test_step$(EXEEXT) test_d02$(EXEEXT) $(EXE)

$(TEST_IO): tests/testio.f90
	$(FC) $(FFLAGS) -Jtests -c $< -o $@

tests/test_unit.o: tests/test_unit.f90 $(TEST_IO) $(MOD_OBJ)
	$(FC) $(FFLAGS) -Itests -c $< -o $@

tests/test_step.o: tests/test_step.f90 $(TEST_IO) $(MOD_OBJ)
	$(FC) $(FFLAGS) -Itests -c $< -o $@

tests/test_d02.o: tests/test_d02.f90 $(MOD_OBJ)
	$(FC) $(FFLAGS) -Itests -c $< -o $@

test_d02$(EXEEXT): tests/test_d02.o $(LIB_OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

test_unit$(EXEEXT): tests/test_unit.o $(TEST_IO) $(LIB_OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

test_step$(EXEEXT): tests/test_step.o $(TEST_IO) $(LIB_OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

test: tests
	python tests/run_tests.py

test-e2e: tests
	python tests/run_tests.py e2e

test-all: tests
	python tests/run_tests.py all

run: $(EXE)
	./$(EXE)

# Works both when make uses cmd.exe and when it uses sh (Git Bash / MSYS / Linux)
ifeq ($(shell echo %OS%),%OS%)
clean:
	rm -f *.o *.mod $(EXE) tests/*.o tests/*.mod test_unit$(EXEEXT) test_step$(EXEEXT) test_d02$(EXEEXT)
else
clean:
	-del /Q *.o *.mod $(EXE) tests\*.o tests\*.mod test_unit$(EXEEXT) test_step$(EXEEXT) test_d02$(EXEEXT) 2>nul
endif

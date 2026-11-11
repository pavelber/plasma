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
#   mo1co2feb.for            - module; element-dependent arrays are ALLOCATABLE,
#                              AllocArrays/AllocLines allocate them at run time
#   Co2feb26_main.for, Co2feb26_procs.for - main program and all procedures of
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
  EXE     := Co2feb26_Generic.exe
  # Static link: the .exe runs on any Windows PC without the MinGW DLLs
  LDSTATIC := -static
else
  EXE     := Co2feb26_Generic
  LDSTATIC :=
endif

# Standard 72-column fixed form, as MS Fortran PowerStation: text after column 72 is ignored
# (several statements in Co2feb26.for have comments there without '!').
# -std=legacy: accept PAUSE and other old extensions without warnings
# OpenMP: parallel loops (number of threads = number of cores, or OMP_NUM_THREADS).
# -fmax-stack-var-size=65536 keeps large local arrays static, as without -fopenmp
# (otherwise -fopenmp moves them to the stack); results are the same with any number
# of threads, see tests/run_tests.py --threads.
OMPFLAGS ?= -fopenmp -fmax-stack-var-size=65536
FFLAGS_COMMON := -std=legacy $(OMPFLAGS)
OPT     ?= -O2
FFLAGS  ?= $(OPT) $(FFLAGS_COMMON)
LDFLAGS ?= $(LDSTATIC)

MOD_OBJ  := mo1co2feb.o
NAG_OBJ  := nag_replacements_Co2_GNU.o
LZ_OBJ   := lzfix_Co2_GNU.o
PROC_OBJ := Co2feb26_procs.o
MAIN_OBJ := Co2feb26_main.o
LIB_OBJS := $(MOD_OBJ) $(NAG_OBJ) $(LZ_OBJ) $(PROC_OBJ)
OBJS     := $(LIB_OBJS) $(MAIN_OBJ)

.PHONY: all debug run clean tests test test-e2e test-all

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

$(PROC_OBJ): Co2feb26_procs.for $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

$(MAIN_OBJ): Co2feb26_main.for $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $< -o $@

debug:
	$(MAKE) -f Makefile.gnu clean
	$(MAKE) -f Makefile.gnu FFLAGS="-O0 -g -fcheck=bounds -fbacktrace $(FFLAGS_COMMON)"

# ---- tests (see tests/run_tests.py and PLAN_refactoring_speed.md)
TEST_IO   := tests/testio.o
tests: test_unit$(EXEEXT) test_step$(EXEEXT) $(EXE)

$(TEST_IO): tests/testio.f90
	$(FC) $(FFLAGS) -Jtests -c $< -o $@

tests/test_unit.o: tests/test_unit.f90 $(TEST_IO) $(MOD_OBJ)
	$(FC) $(FFLAGS) -Itests -c $< -o $@

tests/test_step.o: tests/test_step.f90 $(TEST_IO) $(MOD_OBJ)
	$(FC) $(FFLAGS) -Itests -c $< -o $@

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
	rm -f *.o *.mod $(EXE) tests/*.o tests/*.mod test_unit$(EXEEXT) test_step$(EXEEXT)
else
clean:
	-del /Q *.o *.mod $(EXE) tests\*.o tests\*.mod test_unit$(EXEEXT) test_step$(EXEEXT) 2>nul
endif

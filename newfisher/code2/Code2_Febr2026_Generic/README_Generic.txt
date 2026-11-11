Code2 (Co2feb26), Generic version: one executable for any element
====================================================================

Source: copy of the GNU port in ..\Code2_Febr2026 (Co2feb26_GNU.for, mo1co2feb.for,
nag_replacements_Co2_GNU.f90, lzfix_Co2_GNU.f90, Makefile.gnu) plus the Kr input files.
The originals in ..\Code2_Febr2026 are not changed.

What is different
-----------------
In the original, array sizes were compile-time parameters in mo1co2feb.for:
  nQSm = 1620   (max number of energy levels, must be >= NST)
  HSSm = 36     (max spectroscopic symbol of H-like ion, must be >= HSS)
  Nwork         (= 50 + 12*nQSm + nQSm**2)
  MNLe = 15000  (max number of spectral lines)
so a new element could require editing mo1co2feb.for and recompiling.

Here they are run-time variables:
  nQSm = max(NST) and HSSm = max(HSS), read from lines 2 and 4 of Params0.inp
         at the start of the run (subroutine AllocArrays in mo1co2feb.f90);
  MNLe = number of spectral lines + 1, counted in LineList with the same
         criteria (fluMin, AulMin, hvMin) before the line arrays are filled
         (subroutine AllocLines).
All arrays that depend on them are ALLOCATABLE and are set to zero after
allocation (as the static arrays were).

The input files (Params0.inp, Params1.inp, Params2.inp, QSs*.inp, Exc/Inz/AIw.inp,
InpResp*.inp) have exactly the same format as before; nothing has to be changed in them.
At start the code prints, e.g.
   Params0.inp: nQSm = max(NST) =  1612,  HSSm = max(HSS) =  36;  arrays ~ 0.93 GB

Memory: about 356*nQSm**2 bytes (0.93 GB for NST=1612, 0.44 GB for NST=1107,
1.9 GB for NST=2300). There is no 2 GB static-array limit any more; the limit
is the free RAM.

What is still fixed at compile time (does not depend on the element):
  nXE = 4 (X, C, He, D: the code is written for exactly these 4 elements),
  nvL = 16000 hv points, LaMx = 3 zones, mSpe = 8 frames, mDet = 9 detectors, etc.

Build
-----
  mingw32-make -f Makefile.gnu          -> Co2feb26_Generic.exe
  mingw32-make -f Makefile.gnu debug    -> same with bounds checking
Run: copy Co2feb26_Generic.exe into the folder with the input files of the case
and start it there.

Sources (Nov 2026)
------------------
  Co2feb26_main.f90     main program (time loop)
  Co2feb26_procs.f90    all subroutines and functions
  co2_const.f90         module: constants and fixed dimensions
  co2_atom.f90          module: atomic data, rates, populations, channel lists,
                        AllocArrays/AllocSized/AllocLines/BuildChannels
  co2_util.f90          module: timers, integration context of D01AHF, FlushAll
  mo1co2feb.f90         module: plasma state, grids, spectra; uses the three above
  nag_replacements_Co2_GNU.f90, lzfix_Co2_GNU.f90   as in the GNU port
The former Co2feb26_GNU.for = Co2feb26_main.f90 + Co2feb26_procs.f90, split so that
tests can call the procedures with their own main program.

Faster versions (Nov 2026)
--------------------------
Full runs to 65 ps on 20 cores: Kr 6 min 15 s (5 h 15 min in the first GNU version:
about 50 times faster), Ge 5 min 13 s. All 30 output files of the full Kr and Ge runs are identical byte
for byte to those of the first GNU version.
  1. EmiAbso, bound-free absorption / free-bound emission: the list of BF
     channels (k,kf) is built once per call instead of testing all 1612 x 1612
     level pairs at every one of the ~10500 hv points; same summation order.
  2. VOIGT: its constant tables are computed once instead of in every call.
  3. EED in the BF loop is computed once instead of twice.
  4. OpenMP (all cores, or OMP_NUM_THREADS): BF loop over hv points; BB part of
     EmiAbso (areas of the line profiles over the lines, then the contributions over
     the hv points in the old order of the lines); EffSpIn over hv points; Ws lines,
     ionization and excitation (over lines / initial levels); PMg0 (over levels).
     Each sum keeps its old order, so the results do not depend on the number of
     threads (tested with 1, 3 and 20).
  5. D02EAF: degree-16 Taylor polynomial by the Paterson-Stockmeyer scheme
     (6 matrix products instead of ~13), the last 10 of the s squarings applied to
     the vector, parallel matrix products with a fixed order of summation: about 11
     times faster. This is the only change that moves digits: internal values
     (populations) differ by up to 4e-12 relative; the printed output did not change.
     The previous algorithm is kept as D02EAF_TAYLOR (for the tests).

Other changes that do not change results: sources in free form (.f90; text that was
after column 72 is kept as a marked comment); "do nX = 1, nXkin" (nXkin = 1: kinetics,
lines and BF only for X); PAUSE replaced by CALL Pause1('...'), which behaves as PAUSE
but also writes the message to Warnings.log and stops with exit code 1 (gfortran's
PAUSE without a keyboard silently stopped the run with exit code 0); VOIGT no longer
changes the module variable I; explicit zeros and SAVE for work arrays; the number of
points of each InpResp*.inp is counted from the file; cross sections with explicit
arguments (SigPhiA, SigInzA, SigExcA) and a threadprivate integration context for the
integrands of D01AHF; output files flushed at the end of each time step; a warning in
Warnings.log if not R1 < dc < R2-R1. See PROGRAMMING_ISSUES.md.

Timing
------
Each run writes Timing.log: wall-clock seconds of the main parts of every time step
(Intro, EmiAbso with its BB and BF parts, EffSpIn, PowYie, AtKins = Ws + PMg0 +
D02EAF, Ws split into lines / ionization / excitation / dielectronic capture) and the
totals at the end. The timers do not change any result.
Every run uses all cores: run calculations and tests one after another, not at the
same time (the tests run the Kr and Ge cases one after the other).

Tests
-----
  mingw32-make -f Makefile.gnu test       T1, T1-D02, T2 (about 30 s)
  mingw32-make -f Makefile.gnu test-e2e   T3, 3 time steps of Kr and Ge (about 70 s)
  python tests/run_tests.py all --threads N   with N OpenMP threads
  python tests/run_tests.py all --numeric     numeric comparison (see below)
  python tests/run_tests.py e2e --update      rewrite the golden files (only on purpose)
T1: unit tests of the physics functions (EED, cross sections, VOIGT, D01AHF, D02EAF).
T1-D02: D02EAF against the previous algorithm and against quadruple precision.
T2: one time step of all routines on the real Kr and Ge databases.
T3: the real program, sha256 of all .dat files and of the screen output; tiInf is
    set to 6 ps, so the detailed print-outs at tiInf are tested too.
Comparisons are bit for bit. With --numeric (for changes that move the last digits):
T1/T2 within relative 1e-9, T3 within one unit of the last printed digit against
tests/golden/e2e_*_dat.zip. tests/numcompare.py compares two result folders the same
way. Needs python 3. Golden files: tests/golden.

See also PLAN_refactoring_speed.md (next steps) and PHYSICS_QUESTIONS.md (possible
future physics changes).

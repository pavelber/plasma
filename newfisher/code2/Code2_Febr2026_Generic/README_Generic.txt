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
         at the start of the run (subroutine AllocArrays in mo1co2feb.for);
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
  Co2feb26_main.for     main program (time loop)
  Co2feb26_procs.for    all subroutines and functions
  mo1co2feb.for         module: constants, arrays, AllocArrays/AllocSized/AllocLines
  nag_replacements_Co2_GNU.f90, lzfix_Co2_GNU.f90   as in the GNU port
The former Co2feb26_GNU.for = Co2feb26_main.for + Co2feb26_procs.for, split so that
tests can call the procedures with their own main program.

Faster versions, same results bit for bit (Nov 2026)
----------------------------------------------------
  1. EmiAbso, bound-free absorption / free-bound emission: the list of BF
     channels (k,kf) is built once per call instead of testing all 1612 x 1612
     level pairs at every one of the ~10500 hv points; same summation order.
  2. VOIGT: its constant tables are computed once instead of in every call.
  3. EED in the BF loop is computed once instead of twice.
Full Kr run to 65 ps: 5 h 43 min before, 44 min after 1 and 2. All 30 output
files of full Kr and Ge runs are identical byte for byte to the older version.

Other changes that do not change results: "do nX = 1, nXkin" (nXkin = 1 in the
module: kinetics, lines and BF only for X) instead of "do nX = 1, 1 ! nXE";
PAUSE replaced by CALL Pause1('...'), which behaves as PAUSE but also writes the
message to Warnings.log and stops with exit code 1 (gfortran's PAUSE without a
keyboard silently stopped the run with exit code 0, as a normal end).

Tests
-----
  mingw32-make -f Makefile.gnu test       T1 + T2, about 2 min
  mingw32-make -f Makefile.gnu test-e2e   T3, 3 time steps of Kr and Ge (~10-20 min)
  python tests/run_tests.py e2e --update  rewrite the golden files (only on purpose)
T1: unit tests of the physics functions (EED, cross sections, VOIGT, D01AHF, D02EAF).
T2: one time step of all routines on the real Kr and Ge databases.
T3: the real program, sha256 of all .dat files and of the screen output.
All comparisons are bit for bit. Needs python 3. Golden files: tests/golden.

See also PLAN_refactoring_speed.md (next steps) and PHYSICS_QUESTIONS.md.

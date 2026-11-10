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

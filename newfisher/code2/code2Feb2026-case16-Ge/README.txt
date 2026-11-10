Code2 (Co2feb26), case 16, Ge  --  GNU Fortran version (no NAG library)
=========================================================================

Contents
--------
  Co2feb26_GNU.for              main program (Co2feb26.for, Feb 2026 version, adapted for gfortran)
  mo1co2feb.for                 module mo1co2feb (unchanged)
  nag_replacements_Co2_GNU.f90  replacements of the two NAG routines used by the code:
                                  D01AHF - adaptive 15-point Gauss-Kronrod quadrature
                                  D02EAF - matrix exponential of the rate matrix PM
                                           (PM is frozen during a time step, as in POPdot)
  lzfix_Co2_GNU.f90             post-processing of the output files at the end of the run:
                                  " .ddd" instead of "0.ddd", as the old MS Fortran build printed
  Makefile.gnu                  makefile for gfortran
  Co2feb26.exe                  ready-to-run Windows executable (static, needs no DLLs)
  *.inp, *.INP                  input files for Ge
  Params2_u3D_orig.txt          original Params2.inp (with u3D rows, old Co2nov25 format)
  results\                      output files of the run made with these inputs, and run.log

Changes against the MS Fortran PowerStation + NAG version
---------------------------------------------------------
  1. NAG library (naglib.lib) is not used; D01AHF and D02EAF are in nag_replacements_Co2_GNU.f90.
  2. Co2feb26_GNU.for = Co2feb26.for with
       - two continuation lines (421 and 2438) that started with <TAB>+ rewritten as standard
         fixed-form continuations ('+' in column 6): gfortran does not take <TAB>+ as continuation;
       - CALL FixLeadingZeros() before the final STOP.
  3. The code is compiled as standard 72-column fixed form, like MS Fortran: everything after
     column 72 is ignored (several lines have comments there without '!').

The same build of Code2 for Kr (folder Code2_Febr2026) was compared with the NAG run of the old
build: 14 of 28 output files are identical; in the others about 0.1% of the numbers differ by 1 in
the last printed digit (max. 2-7 units in 10 numbers of 1.47 million).

Params2.inp for Ge
------------------
Co2feb26 reads the Doppler temperature TD [eV] after the Te rows. The Ge Params2.inp had the
old-format rows u3D [cm/s] there. They were converted to

    TD = Ti + (pi/8) * M * u3D^2 * amu / e        (M = 72.6 for Ge, Ti = Te)

which gives exactly the same Gaussian (Doppler) width of the Ge lines as the old formula
sqrt(fwTi^2 + fwU^2). For C, He and D lines the new code uses TD/M with the same TD, so their widths
are not the same as in the old u3D formula. The original file is Params2_u3D_orig.txt.

How to compile (Windows)
------------------------
1. Install GNU Fortran, e.g. WinLibs (MinGW-w64 UCRT), which also has mingw32-make:
       winget install BrechtSanders.WinLibs.POSIX.UCRT
   (this version was built with GNU Fortran 16.1.0). Open a new terminal afterwards.
2. In this folder:
       mingw32-make -f Makefile.gnu
   This produces Co2feb26.exe (statically linked: it runs on any Windows PC without MinGW).
   Other targets:
       mingw32-make -f Makefile.gnu clean     remove *.o, *.mod and the exe
       mingw32-make -f Makefile.gnu debug     build with bounds checking and backtrace

   Without make, the same build by hand (the module must be compiled first):
       gfortran -O2 -std=legacy -c mo1co2feb.for
       gfortran -O2 -std=legacy -c nag_replacements_Co2_GNU.f90
       gfortran -O2 -std=legacy -c lzfix_Co2_GNU.f90
       gfortran -O2 -std=legacy -c Co2feb26_GNU.for
       gfortran -O2 -o Co2feb26.exe mo1co2feb.o nag_replacements_Co2_GNU.o lzfix_Co2_GNU.o Co2feb26_GNU.o -static

Linux: the same with "make -f Makefile.gnu" (the executable is called Co2feb26). Note that on
Linux file names are case sensitive: the code opens Exc.inp, Inz.inp and AIw.inp, while the Ge
files are called Exc.INP, Inz.INP and AIW.INP -- rename them there.

How to run
----------
Run Co2feb26.exe in the folder with the input files (cmd or PowerShell):
       Co2feb26.exe
The output files (*.dat) are written to the current folder. The run ends with
       STOP    ti >= StopTime.  The End.
The full run (0 - 65 ps, 27 time steps) takes about 4 hours on one core.
For a quick check (one time step, ~10 min) set the end-of-computation time in line 5 of
Params2.inp to 1.d-12.

If Windows shows "Windows protected your PC / unrecognized app" after the zip was downloaded:
before unpacking, right-click the zip -> Properties -> tick "Unblock" -> OK
(or in PowerShell:  Unblock-File .\<name>.zip ).  The exe is not signed; this is the usual
SmartScreen reaction to any self-built program.

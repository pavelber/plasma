!  Code2 (Co2feb26), Generic version. Module split (Nov 2026):
!    co2_const  - constants and fixed dimensions
!    co2_atom   - atomic database of the elements, rate matrices and populations,
!                 channel lists; their allocation (AllocArrays, AllocSized, AllocLines)
!    co2_util   - wall-clock timers, integration context of D01AHF, FlushAll
!    mo1co2feb  - state of the plasma, grids, spectra, run control; it uses the three
!                 modules above, so 'use mo1co2feb' gives access to everything.
      module co2_const
      implicit none
      integer, public, parameter :: &
     &     nXE= 4  ! number of chemical elements (XE) in present simulation:
!                                nX=1,2,3,4 is Kr,C,He,D along ion mass, see Params0.inp.
      integer, public, parameter :: &
     &   nXkin = 1  ! kinetics, lines and BF only for X (nX=1); C, He, D excluded
      integer, public, parameter :: &
     &   nvL  = 16000, &  ! Reserved number of hv-grid points. True number of v-points "nvM" <= "nvL" in defined in "vGrid" subr"
     &   LaMx = 3, &  ! Number of types of zones
     &   mSpe = 8, &  ! Number of frames (spectrograms) or radial images.
     &   ntp= mSpe+2, &  ! Number of t-points in Scenario
     &   mDet = 9, &  ! Number of diagnostics (4 spectrometers + 2 imaging cameras + 3 power detectors)
     & mxPoRes= 5000, &  ! Reserved number for hv points in input files of spectral response of each detector
     & LOSnMx = 100, &  ! Reserved number for radial zones in computation of from-the-target radiation power
     & diLOSn = 3, &  ! Number of "diagnostic" LOSs
     &   imgs = 2, &  ! Number of imaging cameras
     &    nPD = 3, &  ! number of Power Detectors
     &  K13po = 119  ! Number of points in linear interpolation of spherical K13(k*R) function

      real(8), public, parameter :: &
     &   c    = 2.99792458d10, &  ! cm/s  ADT
     &   elma = 9.1093897d-28, &  ! g; electron mass
     &   Uam  = 1.6605402d-24, &  ! g; atomic mass unit
     &   e2   = 23.07077d-20, &  ! CGS; note e2 = har[eV] * a0[cm] *BolEr
     &   a0   = 0.52917725d-8, &  ! cm, Bohr radius
     &   BolJ = 1.6021773d-19, &  ! J/eV  Boltzmann
     &   BolEr= 1.6021773d-12, &  ! Erg/eV  Boltzmann
     &   RyeV = 13.605698, &  ! eV, Rydberg E, Martin-Wiese, Atomic Spectroscopy (3)
     &   har  = 2.d0*RyeV, &  ! eV; hartree, AU unit of energy
     &   hPL  = 4.1356692d-15, &  ! eV*s;  Planck
     &   pin  = 3.14159265d0, &
     &   ToPi = 2.d0*pin, &
     &   FoPi = 4.d0*pin, &
     &   hBar = hPL/ToPi, &  ! eV*s;  Planck bar
     &   eVA  = 12398.424d0, &  ! A*eV;  hv(eV)*Lambd(A)
     &   Ampli= 2.362d-13, &  ! 8*pi/sqrt(3) * pi*ao^2 * Ry^2  for VR
     &   zero = 0.d0, &
     &   one  = 1.d0, &
     &   two  = 2.d0, &
     &  third = one/3.d0, &
     &   sqpi = 1.77245385d0, &  ! sqrt(pin)
     &   CoSp = 3.d0/FoPi, &  ! for volume of sphere
     &   tol  = 1.d-3, &  ! tolerance in NAG d01;  must be <= 10^-7. Checked by using equal GT for cells #1 & #3 in z1518 with RF=0
     &  tolD02= 1.d-7, &  ! tolerance in NAG d02;  NOTE: at some t-step it gave "SumPOP-1 > d-8", thus I allowed "SumPOP-1" up to d-7, see "My STOP after d02"
     &   MuAU = Uam/elma  ! proton mass in e-mass units, i.e. in AU

      end module co2_const

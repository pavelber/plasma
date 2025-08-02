C      globals.inc - Shared parameters and variables for the plasma code
C
C      --- Parameters ---
       INTEGER, PARAMETER :: NSTm=1612, nvM=12300, LaMx=2, mSpe=6
       INTEGER, PARAMETER :: MNLe=11566, nXE=2, HSSm=36
       REAL(8), PARAMETER :: pin=3.1415926535d0, one=1.d0, zero=0.d0
       REAL(8), PARAMETER :: ToPi=2.d0*pin, FoPi=4.d0*pin
       REAL(8), PARAMETER :: BolJ=1.6021773d-19, h=4.1356692d-15
       REAL(8), PARAMETER :: eVA=12398.424d0
C
C      --- Plasma State Common Block ---
       COMMON /STATE/ La, tf, ti, Te, TiD, DenI, Dene, R2, ZC1, ZC2, ng
       INTEGER La, ng
       REAL(8) tf, ti, Te, TiD, DenI(2), Dene, R2, ZC1, ZC2
C
C      --- Population and Energy Level Data ---
       COMMON /LEVELS/ POPt, E, g0, BE, A, flu, Wout
       REAL(8) POPt(NSTm, nXE, LaMx), E(NSTm,nXE), g0(NSTm,nXE)
       REAL(8) BE(NSTm), A(NSTm,NSTm,nXE), flu(NSTm,NSTm,nXE)
       REAL(8) Wout(NSTm,nXE,LaMx)
C
C      --- Atomic and Line Data Common Block ---
       COMMON /LINES/ hvC, nLo, nUp, nX3, linM4, hvV
       INTEGER nLo(MNLe), nUp(MNLe), nX3(MNLe), linM4
       REAL(8) hvC(MNLe), hvV(nvM)
C
C      --- Ion and Species Data ---
       COMMON /SPECIES/ Den, Nnu, FSS, HSS, NST, kiSS, nprin
       INTEGER Nnu(nXE), FSS(nXE), HSS(nXE), NST(nXE)
       INTEGER kiSS(NSTm,nXE), nprin(NSTm,nXE)
       REAL(8) Den(nXE)
C
C      --- Spectral Data ---
       COMMON /SPECTRAL/ FWkVcGAU, AtMass, PI, PIR, DPI, hvSmo, hvMax
       REAL(8) FWkVcGAU(nXE), AtMass(nXE), PI(HSSm,nXE)
       REAL(8) PIR(HSSm,nXE), DPI(HSSm,nXE), hvSmo, hvMax
C
C      --- Output Arrays Common Block ---
       COMMON /OUTPUT/ emTot, abTot, FWevLor, FWvoi
       REAL(8) emTot(LaMx,nvM), abTot(LaMx,nvM)
       REAL(8) FWevLor(MNLe), FWvoi(MNLe)
C
C      --- Names and Control ---
       COMMON /NAMES/ QSname1, QSname2, gpEq, PrFr
       CHARACTER*5 QSname1(NSTm,nXE), QSname2(NSTm,nXE)
       INTEGER gpEq(mSpe), PrFr

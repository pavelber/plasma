      module mo1co2feb 
      implicit none
      integer, public, parameter :: 
     +     nXE= 4              ! number of chemical elements (XE) in present simulation: 
c                                nX=1,2,3,4 is Kr,C,He,D along ion mass, see Params0.inp. 
      integer, public :: 
     +     FSS(nXE), HSS(nXE), ! First & H-like Spectroscopic Symbol (SpS) in database (DaBa); FSS can be any, but it must have > 1 energy levels (ELs)
     +     Nnu(nXE),           ! nuclei order # in databases = the number of "non-AI" Energy Levels (ELs) 
     +     NST(nXE)            ! number of ELs in each of 4 DaBa [non-AI and AI] : .

c  Array sizes that depend on the chemical element are NOT compile-time
c  parameters in this Generic version: AllocArrays() (CONTAINS part below)
c  sets nQSm= max(NST), HSSm= max(HSS) from Params0.inp at run time,
c  AllocLines() sets MNLe from the number of spectral lines (LineList).
      integer, public ::
     +     nQSm = 0,      ! number of ELs: max(NST(1:nXE)) of Params0.inp
     +     HSSm = 0,      ! max SpS of H-like ion: max(HSS(1:nXE)) of Params0.inp
     +    Nwork = 0,      ! 50+ 12*nQSm+ nQSm**2, for NAG d02eaf work Area WEAF
     +     MNLe = 0       ! number of spectral lines + 1, set in LineList
      integer, public, parameter ::
     +   nXkin = 1   ! kinetics, lines and BF only for X (nX=1); C, He, D excluded
      integer, public, parameter :: 
     +   nvL  = 16000, ! Reserved number of hv-grid points. True number of v-points "nvM" <= "nvL" in defined in "vGrid" subr"
     +   LaMx = 3,     ! Number of types of zones
     +   mSpe = 8,    ! Number of frames (spectrograms) or radial images.
     +   ntp= mSpe+2, ! Number of t-points in Scenario 
     +   mDet = 9,    ! Number of diagnostics (4 spectrometers + 2 imaging cameras + 3 power detectors)
     + mxPoRes= 5000, ! Reserved number for hv points in input files of spectral response of each detector 
     + LOSnMx = 100,  ! Reserved number for radial zones in computation of from-the-target radiation power
     + diLOSn = 3,    ! Number of "diagnostic" LOSs    
     +   imgs = 2,    ! Number of imaging cameras    
     +    nPD = 3,    ! number of Power Detectors
     +  K13po = 119   ! Number of points in linear interpolation of spherical K13(k*R) function 
       
      real(8), public, parameter ::    
     +   c    = 2.99792458d10,  ! cm/s  ADT  
     +   elma = 9.1093897d-28,  ! g; electron mass
     +   Uam  = 1.6605402d-24,  ! g; atomic mass unit
     +   e2   = 23.07077d-20,   ! CGS; note e2 = har[eV] * a0[cm] *BolEr
     +   a0   = 0.52917725d-8,  ! cm, Bohr radius 
     +   BolJ = 1.6021773d-19,  ! J/eV  Boltzmann
     +   BolEr= 1.6021773d-12,  ! Erg/eV  Boltzmann
     +   RyeV = 13.605698,      ! eV, Rydberg E, Martin-Wiese, Atomic Spectroscopy (3) 
     +   har  = 2.d0*RyeV,      ! eV; hartree, AU unit of energy
     +   hPL  = 4.1356692d-15,  ! eV*s;  Planck
     +   pin  = 3.14159265d0,
     +   ToPi = 2.d0*pin, 
     +   FoPi = 4.d0*pin, 
     +   hBar = hPL/ToPi,       ! eV*s;  Planck bar        
     +   eVA  = 12398.424d0,    ! A*eV;  hv(eV)*Lambd(A)
     +   Ampli= 2.362d-13,      ! 8*pi/sqrt(3) * pi*ao^2 * Ry^2  for VR       
     +   zero = 0.d0,
     +   one  = 1.d0,
     +   two  = 2.d0,			                          
     +  third = one/3.d0,
     +   sqpi = 1.77245385d0,  ! sqrt(pin)
     +   CoSp = 3.d0/FoPi,     ! for volume of sphere
     +   tol  = 1.d-3,     ! tolerance in NAG d01;  must be <= 10^-7. Checked by using equal GT for cells #1 & #3 in z1518 with RF=0
     +  tolD02= 1.d-7,     ! tolerance in NAG d02;  NOTE: at some t-step it gave "SumPOP-1 > d-8", thus I allowed "SumPOP-1" up to d-7, see "My STOP after d02"  
     +   MuAU = Uam/elma   ! proton mass in e-mass units, i.e. in AU        

      character*24, public, allocatable :: QSname(:,:)  ! part 1, according to 'Inz.inp' list
      character*9,  public :: title, empty, comme  ! in input files reading

      integer, public, allocatable :: 
     +  nuAS(:,:),   ! Number of AutoIonizing (AI) ELs in each SpS
     +  nuQS(:,:),   ! Number of non-AI ELs in each SpS via "QSs.inp"
     +  nuGS(:,:),                ! EL# of ground state ("GrSt")'s of each SpS, including nucl.
     +  kAI1(:,:), kAI2(:,:),  ! for AI ELs of each SpS, their first & last # in total list of ELs
     +  kiSS(:,:),  pqn(:,:),  ! SpS of each EL & XE;  For each EL: principal quantum number of outer shell
     +  nu1(:), nu2(:),            ! Level order # in Z-stage, Level order ## in total list
     +  eqEL(:,:),  orb(:,:),  ! number of equivalent electrons in nl-subshell; orbital q.n.
     +  MthdEX(:,:)               ! formula # for fitting e-impact excitation cross-section of high-Z dopant 
      integer, public, allocatable :: 
     +  nX3(:), nUp(:), nLo(:), ! XE and EL serial numbers of Upper & Lower levels of spectral line after ordering the lines over hv
     +  nUph(:,:), nLoh(:,:)  ! same prior to the ordering
      integer, public :: 
     +  LastAI(nXE),                     ! EL-List number of the last AI EL;  calculated in "Intro"
     +  linM(nXE),    ! number of spectral lines in [hvmin, hvmax] domain
     +  AskInf(100),  ! number of Kr energy levels to be given PM-info in file (49                  
     +  npInpRes(mDet), ! number of hv points in input file of Spectral Response of each detector, see DATA  
     +  nqInf,          ! For how many QSs of Xx (<101) you want info regarding its contacts with other QSs?  File "Contacts.dat"
     +  LaInf,          ! For what cell# ?
     +  Nr,    ! number of spherical layers IN EACH ZONE
     +  Ntet,  ! number of sectors over polar angle 
     +  linM4, ! number of spectral lines in [hvmin, hvmax] domain (sum over XE) 
     +  La,    ! serial # of plasma layer under study (w-layer)
     +  nX,    ! variable (current) number of Chemical Element (XE)
     +  NCL,   ! number of coaxial cylindrical tubes == number of concentrical radial belts on 
c                       seen-to-detector plane round image of target, Figs.1,2.
     +  nvM, LastFr,             ! number of hv-points in full interval, see subr "hvPoints"   
     +  k, kf, j, i, iX, iv, iw, ! variable integers
     +  kw, k7, k1, k2, kU, kL,  
     +  iSS, jSS, kQS, kS, nw,   ! variable integers
     +  lin, nLam, nXw, n7, lw,  ! variable integers
     +  npts, Nlim, Ifail,       ! integers in D01ahf and d02
     +  nuBSs,                   ! number of BSs in the core  
     +  Count, StrExc, CountExc,  
     +  StrInz, CountInz, 
     +  StrAIw, CountAIw, CoFr,
     +  nGaLor                  ! number of frame to be convolved with Gaussian and Lorentzian instrumental functions.

      real(8), public :: 
     +  diSpIn(nvL, diLOSn), ! Spectral Intensity on exit of "diLOSn" diagnostic LOSs from target at ti=tiInf
     +    SpIn(nvL, LOSnMx), ! Spectral intensity of radiation on exit from the target along "NCL" parallel LOSs. [W/eV/sr/cm2] 
     +   FrYie(nvL, mSpe),   ! array of 8 frames [J/eV/sr] 
     +    Resp(nvL, mDet), ! on hv array of the code ("hvV") Spectral Response of each of "mDet" detectors .
     +    SpeY(nvL),       ! t-depe spectral yield [J/eV/sr] gathered since t0 till "StopTime" 
     +     hvV(nvL),       ! [eV] array of hv points
     +   SpPow(nvL)      , ! [W/eV/sr] Spectral power of radiation from the target 
     +   SpInEf(LaMx,nvL), ! OMEGA-mean RF intensity in 2 zones [W/cm2/sr/eV]

     +  absoFF(nvL), emisFF(nvL),  
     +  absoBF(nvL), emisFB(nvL),  
     +  absoBB(nvL), emisBB(nvL),  
     +  EmTot(LaMx, nvL),         ! plasma emissivity [W/cc/sr/eV] in LaMx shells 
     +  AbTot(LaMx, nvL),         ! plasma absorption coefficient corrected for stimulated emission [1/cm] in the same shells
     +  emisFBx(nvL,nXE),         ! in EmiAbso subr
     +  absoBFx(nvL,nXE),  

     +  rc(LOSnMx),  Ar(LOSnMx), 
     +  Image(imgs, LOSnMx), 

     + hvRes(mxPoRes,mDet),    ! hv array of input file "InpResp1.inp"   
     + InRes(mxPoRes,mDet)    ! on this "input" hv array, spectral response of "mDet" detectors
      real(8), public, allocatable :: 
     +     LvJW(:,:,:),  ! LEVEL Stark width [eV], J-formulary 
     +   LvLorW(:,:,:),  ! same corrected for uncertainty principle
     +    PI(:,:),            ! Ionization Potential of each GS, table value
     +   PIR(:,:,:),  ! Ionization Potential of each GS, reduced by continuum lowering
     +   DPI(:,:,:),  ! Continuum lowering

     +     E(:,:),       ! EL Energy relative to THIS-SS ground state
     +    g0(:,:),       ! EL degeneracy: table value
     +    BE(:,:,:),  ! Binding Energy of each EL 'ti'-corrected in 'AtKins' after continuum lowering
     +   BEp(:,:,:),  ! after 'call AtKins', 'tf'-Binding Energy of w-layer is saved until next visit to this 'La'; then previous-t BE may be needed for restart of dead EL.
     +   bra(:,:,:),  ! with FAC bases, we assign bra= 1/0 based on yes/no single-e ioniz cross-Sec for this i/f couple in "InzXE....inp" file.  
     +     A(:,:,:),  ! Einstein A coef [1/s]
     +  WAiz(:,:,:),  ! AutoIoniz Probability [1/s]
     +   flu(:,:,:),  ! Correct-sign (>0) Absorption Oscillator Strength
     +    Ax(:,:),      ! Ax-Fx are 6 coefs used in fitting formulae for 
     +    Bx(:,:),      ! e-impact excitation cross-sections of 
     +    Cx(:,:),      ! high-Z dopant, see Exc.inp 
     +    Dx(:,:),
     +    Ex(:,:), 
     +    Fx(:,:),
     +   Aix(:,:),  ! Aix-Dix are 4 coefs of e-impact ioniz cross-section of high-Z dopant, see "Inz.inp"
     +   Bix(:,:),  
     +   Cix(:,:), 
     +   Dix(:,:),
     +   Eix(:,:),  ! Eix-Hix are 4 coefs of photo-ioniz cross-section of high-Z dopant, see "Inz.inp"
     +   Fix(:,:), 
     +   Gix(:,:), 
     +   Hix(:,:), 
     +   Eth(:,:),  ! State-to-state Ionization Threshold (for ioniz cross-secs) 
     +    WI(:,:),                   ! single ionization probability (1/s), removal of one electron per e-impact
     +   WEX(:,:),  WDX(:,:),  ! e-impact excitation & deexcitation probabilities (1/s)
     +   WRR(:,:),  WTB(:,:),  ! radiative & 3-body recombination probabilities (1/s)
     +  WInd(:,:),  Wab(:,:),  ! induced emission & Photo(BB)absorption probabs (1/s)
     +  WPhI(:,:), WiRR(:,:),  ! photoIonization & induced RR probabs, 1/s
     +   WDC(:,:),                   ! probability of Dielectronic Capture into AI EL of He-like & Li-like ions
     +   EAI(:,:),   ! FACn energy of AI transition (column "DE[eV]" in "AIw.inp)"    
     +    PM(:,:),   ! Probability Mtrx [1/s], t-Scale=10^40 for d02
     +  Wout(:),             ! EL depletion rate, PM(k,k)= -Wout(k)  
     +  WoutXE(:,:,:),  ! "Wout" remembered for each XE for Stark
     +  POP(:),              ! relative abundance of QSs
     +  POPi(:,:,:),  ! ti-values of EL POP saved La,nX-dependent 
     +  POPf(:,:,:),  ! save as 'tf" value for loading in POPs in Scenario for next ti
     +  POPZ(:,:,:),  ! relative abundance of ionization stages, including nucl, which has SS= HSS+1 
     +  WEAF(:)                ! NAG params and WorkArea (Nwork)
      real(8), public :: 
     +  HoF(nXE+1),             ! Holtzmark Fo-field [a.u.] due to i of one XE or electrons, J (3.49) 
     +  niFra(nXE,LaMx),        ! fraction of XE in total ion number density DenI(La) [i/cc]
     +    FrP(mSpe), ! time [s] of centers of "mSpe" frames  
     +  diFrP(mSpe), ! time [s] of "mSpe" frames from "diagnostic" LOSs  
     +   tPo(ntp),  ! t-points including t=0 and 1 point after last spectra printout
     +   R1t(ntp),  ! radius [cm] of BSs in the t-point #tip
     +   R2t(ntp),  ! radius of Core    [cm] in the t-point #tip
     +   R3t(ntp),  ! radius of Capsule [cm] in the t-point #tip
     + nuBSst(ntp), ! number of BSs in the core  
     +   W1D(ntp),            ! w-array for reading strings of Params2.inp
     +   nit(ntp, LaMx, nXE), ! t-points of ion number density of each XE [i/cc] in La
     +   Tet(ntp, LaMx),      ! t-points of Te [eV] in La
     +   TDt(ntp, LaMx),      ! t-points of Doppler temperature [eV] in La
     +   bpt(ntp, LaMx),      ! t-points of e-beam part of EED in La
     +   bct(ntp, LaMx),      ! t-points of e-beam center [eV] in e-beam part of EED in La
     +   bwt(ntp, LaMx),      ! t-points of e-beam width  [eV] in e-beam part of EED in La
     +   ceR(LaMx),      ! [cm] outer radius of spherical zone            
     +   Te(LaMx),    ! electron  temperature in La-layer [eV], prescribed in Scenario
     +   TD(LaMx),    ! Doppler temperature in La-layer [eV], prescribed in Scenario
     +   Tion(LaMx),  ! ion temperature (is used only for Stark width) 
     +   Den(nXE,LaMx), ! ion number density [i/cc] of all XE in each "La"   
     +   DenI(LaMx),    ! total (all-XE) ion density, i/cc  
     +   Dene(LaMx),    ! electron density, e/cc
     +   ZC(nXE,LaMx),            ! each-XE mean ion charge  
     +   ZC1(LaMx), ZC1pr(LaMx),  ! mean (all-XE, all-SS, inclu atoms) ion charge 
     +   kRp(k13po), Ksp(k13po),  ! 119-point fit to spherical K13 function Ks(kR) 
     +   WSr(nXE+1),              ! Wigner-Seitz radius for ions of same XE or electrons
     +   Deb(nXE+1), ! Debye radius via ions of one XE or electrons
     +   DeF(nXE+1), ! "Full" Debye radius of XE [(3.5) via all spacies lighter than XE or equal to XE]
     +   ipp(nXE+1),     ! JS-factor ita/pp (3.51a)   
     +   irp(nXE+1,nXE), ! JS-factor ita/rp (3.51b) depends on both: emitter & perturber   
     +   fre(nXE+1,nXE), ! microfield frequency due to perturbers of one type JS (3.50), depends on both: emitter & perturber 
     +   Wqs(nXE+1),     ! Level quasistatic Stark width due to perturbers of one type (ions of each XE & electrons); JS (3.51) 
     +   vel(nXE+1),  ! A.U., thermal velo of perturbing ions or electrons as defined by JS (3.11)
     +   R352(nXE+1), ! ratio JS (3.52)
     +   f353(nXE+1), ! ratio JS (3.53)
     +   Sta(nXE+1),  ! A.U., LEVEL Stark width due to perturbers of one type JS (3.54)
     +   Sah(LaMx),   ! Sah = Dene(La)*(one-bp(La))/(6.03717d21*Te(La)**1.5d0)
     +   SigMax(LaMx), ! Geom upper limit required in ionization because of assumed scaling over X=E/BEk in FACn Sig(X) while BEk can drop low at high density  
     +   DiAU(nXE),    ! ion density in "ions per a0^3", i.e. ni [i/cc] * 1.48185e-25 cc 
     +   AtMass(nXE),     ! ion mass in [a.u.] & [g].   Amass= AtMass*1.66054d-24 
     +   ExInzE, ExInzR,  ! Ioniz+Excit part of Internal E [eV/HP] & it's Growth rate [eV/s/HP]
c     +   fwTiKvC(LaMx,nXE), 
c     +   fwTUKvC(LaMx,nXE),   ! for-broadening info
     +   FWkVcGAU(LaMx,nXE),  ! FWHMgau/hvC via TD, same for all lines of XE. 
     +   Xc(LaMx), uC(LaMx),  ! X-coord of the La-center & plasma flow velocity re-calculated to the cell center 
     +   dis(mSpe),           ! distance to periph LOS in TREX scans
     +   DiZ2(LaMx),                             ! XE-sum of Den*Z^2 for FF emission
     +   Zdot(LaMx), ZdotPr(LaMx), ZdotN(LaMx),  ! XE-mean dZ/dt; revious
     +   dZpdt(nXE),                             ! XE-resolved dZ/dt
     +   bp(LaMx), bc(LaMx), bw(LaMx),  ! params of e-beam for non=Maxw EED (given by "bp > 0."), see EED function 
     +   hehv(30), dvKv(30),            ! hard edge of domain in hvV(iv) array, between-points interval (dv/v) in each domain 
     +   rdi(diLOSn),        ! distanse between r=0 and "diagnostic" LOS  
     +   hvPrint1, hvPrint2, ! [eV] spectral edges of Print-Out interval of "Frames.dat"  
     +   hvPrint3(diLOSn),   ! [eV] soft edges of hv-intervals resolved by spectrometers 2,3,4 (here "diLOSn"=3 "diagnostic" LoSs)
     +   hvPrint4(diLOSn),   ! [eV] hard edges of 
     +   PDsign(nPD),    
     +   PowDet, 
     +   hveV, LambA,  
     +   wBS, diBSs,   ! mean distance between centers of neibouring BS.	(II.1a)
     +   dc,           ! distance from center of test BS to surface of core, Fig.3.
     +   tiInf,        ! time to display "...LineInfo.dat", "...EmiAbso.dat", "EffSpIns.dat", "AlongCeLOS.dat"  
     +   domNu,        ! number of domains in array of photon energy points, hvV(iv) 
     +   hvMin, hvMax, ! edges of the v-grid [eV] from FLAG
     +   totSt,        ! A.U., LEVEL Stark width due to all e,i 
     +   wwr, tiS, tfS, ! work ratio
     +   TeAU, TiAU,    ! Te, Ti in A.U. = hartree = 2*Ry = 27.211396 eV
     +   DeAU,       ! electron density in "e per a0^3", i.e. ne [e/cc] * 1.48185e-25 cc 
     +   Amass,      ! ion mass [g],   Amass= AtMass(nX)*1.66054d-24 
     +   AulMin,     ! Min allowed value of Einst A() coef. If A() < AulMin, we replace it by A() =0 in "Intro" 
     +   fluMin,     ! Min allowed value of Abso OscStrenght. If f() < fluMin, we replace A bt 0 in "Intro" 
     +   DE, BEk,    ! E betw two levels under consideration, E required for any 'k'-->'kf' transition, init energy of free el in fb trans. 
     +   stopTime,     ! time to stop the run [s].  Instant chosen for CRE calculations, both from FLAG 
     +   SumP, Scale,  ! sum POP 
     +   tstep, ti, tf,   ! time-step [s]. Initial & final instants of present time-step: tf= ti+ tstep; tm=(ti+tf)/2. Start-value of "ti".
     +   strt, tm, rer,   ! used in D01ahf(...)
     +   TeDotFrE,        ! part of 'dTe/dt', namely, what is due to thermalization of fresh electrons to 'Te'
     +   Zw, wtot, wtot1, ! w-var for Zbar and CPpCm in 'AtKins' 
     +   POPk, POPkr,     ! used for PhI with  
     +   rPSI, distII,    ! used in functions SigExc, SigInz, SigPhi for avoiding too large values via FAC fit errors.  No corre in REVERSE processes as (1) they are expre via direct & (2) must provide equilibration.             
     +   upTB, Incr, frac,  
     +   ReduReso, ReReiv,    
     +   R1, R2, R3,      ! [cm] more practical than "CeR(La)"; t-dependent  
     +   L1, L2, L3, 
     +   So1, So2, So3, 
     +   ta1, ta2, ta3,    
     +   uTi, nDE,       ! [cm/s] mean thermal velo of ion
     +   FrL,            ! duration of x-ray frame;   
     +   W2,W2max,       ! probability of crossing 2 BSs by 1 LOS, maximal   
     +   hvKey, WingCut, ! restricts hv-length of line wing via Wing = hvC(line)*WingCut      
     +   Wing, dhv, ww,  
     +   MaxExch, POPexch, 
     +   AsumU, AsumL, Asum,
     +   minGaLor, maxGaLor, ! Edges of instr-conv interval [eV]    
     +   Ains, Bins, Cins    ! constants of quadratic fit to FWHM/hv of instrumental Gaussian (or Lorentzian),

      real(8), public, allocatable ::
     +   Norm(:,:),    ! (MNLe,LaMx) integral of the line shape: it becomes < 1 after hard wings cut & must be used for re-norm of the shapes to 1
     +   hvCh(:,:),    ! (MNLe,nXE) Spectral line Centers before hv-regulation
     +   hvC(:),       ! (MNLe)     Spectral line Centers after hv-regulation
     +   QMlimit(:,:), ! (MNLe,LaMx) lower limit for level width from uncertainty princip
     +   FWevLor(:,:)  ! (MNLe,LaMx) Lorentzian (Stark) part of FWHM [eV]

c     npInpRes (number of hv points in each InpResp*.inp) is counted in OpenFiles

      DATA kRp /0., 
     +       3.52E-4,  4.05E-4,  4.65E-4,  5.35E-4,  6.15E-4,  7.08E-4,  ! tau-points of our 119-point fit to spherical K13(tau)  
     +       8.14E-4,  9.36E-4,  0.00108,  0.00124,  0.00142,  0.00164, 
     +       0.00188,  0.00216,  0.00249,  0.00286,  0.00329,  0.00379,
     +       0.00435,  0.00501,  0.00576,  0.00662,  0.00761,  0.00876, 
     +       0.01007,  0.01158,  0.01332,  0.01532,  0.01761,  0.02025, 
     +       0.02329,  0.02679,  0.0308,   0.03542,  0.04074,  0.04685, 
     +       0.05388,  0.06196,  0.07125,  0.08194,  0.09423,  0.10837, 
     +       0.12462,  0.14331,  0.16481,  0.18953,  0.21796,  0.25066, 
     +       0.28825,  0.33149,  0.38122,  0.4384,   0.50416,  0.57978, 
     +       0.66675,  0.76676,  0.88178,  1.01405,  1.16615,  1.34108, 
     +       1.54224,  1.77357,  2.03961,  2.34555,  2.69738,  3.10199, 
     +       3.56729,  4.10238,  4.71774,  5.4254,   6.23921,  7.17509, 
     +       8.25135,  9.48905, 10.91241, 12.54928, 14.43167, 16.59642, 
     +      19.08588, 21.94876, 25.24107, 29.02724, 33.38132, 38.38852, 
     +      44.14679, 50.76882, 58.38414, 67.14176, 77.21302, 88.79497, 
     +     102.11422,  117.43135,  135.04605,  155.30297,  178.59841, 
     +     205.38817,  236.19637,  271.62587,  312.36974,  359.22522, 
     +     413.10892,  475.07534,  546.33664,  628.28713,  722.53018, 
     +     830.90968,  955.54608, 1098.87817, 1263.70977, 1453.26626, 
     +    1671.25626, 1921.94475, 2210.23634, 2541.77194, 2923.03773, 
     +    3361.49352, 3865.7171,  1E20 / 

      DATA Ksp / 0.75,    0.75,    0.74999, 0.74999, 0.74999, 0.74999,  ! f-points of our 119-point fit to spherical K13(tau)  
     +           0.74999, 0.74998, 0.74998, 0.74998, 0.74997, 0.74997, 
     +           0.74996, 0.74996, 0.74995, 0.74994, 0.74993, 0.74992, 
     +           0.74991, 0.74990, 0.74988, 0.74986, 0.74984, 0.74982, 
     +           0.74979, 0.74976, 0.74972, 0.74968, 0.74963, 0.74957, 
     +           0.74951, 0.74944, 0.74936, 0.74926, 0.74916, 0.74903, 
     +           0.74890, 0.74874, 0.74857, 0.74837, 0.74815, 0.7479, 
     +           0.74763, 0.74733, 0.747,   0.74665, 0.74627, 0.74587, 
     +           0.74546, 0.74506, 0.74467, 0.74434, 0.74408, 0.74396, 
     +           0.74402, 0.74436, 0.74507, 0.74626, 0.7481,  0.75074, 
     +           0.75438, 0.75922, 0.76546, 0.77327, 0.78278, 0.79402, 
     +           0.80689, 0.82116, 0.83644, 0.85222, 0.86794, 0.88308, 
     +           0.89719, 0.91003, 0.92149, 0.9316,  0.94046, 0.94819, 
     +           0.95492, 0.96079, 0.96591, 0.97036, 0.97423, 0.9776, 
     +           0.98053, 0.98308, 0.98531, 0.98724, 0.98892, 0.99039, 
     +           0.99167, 0.99278, 0.99375, 0.9946,  0.99533, 0.99598, 
     +           0.99654, 0.99703, 0.99746, 0.99784, 0.99817, 0.99846, 
     +           0.99871, 0.99893, 0.99913, 0.99929, 0.99944, 0.99956, 
     +           0.99966, 0.99975, 0.99982, 0.99987, 0.99992, 0.99995, 
     +           0.99997, 0.99998, 0.99999, 1.0,     1.0  /

      contains


      SUBROUTINE AllocArrays()  ! Generic version: sizes of atomic-data arrays at run time.
c  Reads lines 1-4 of Params0.inp (FSS, HSS, Nnu, NST of the nXE elements) on its own unit;
c  Intro() reads the same lines again from unit 12 as before, so Params0.inp is unchanged.
      integer iu, ios, w4(nXE), mHSS(nXE), mNST(nXE)
      real(8) GB
      open(newunit=iu, file='Params0.inp', status='old', iostat=ios)
      if(ios.ne.0) STOP 'AllocArrays: cannot open Params0.inp'
      read(iu,*) w4      ! FSS
      read(iu,*) mHSS    ! HSS
      read(iu,*) w4      ! Nnu
      read(iu,*) mNST    ! NST
      close(iu)
      if(maxval(mHSS).lt.1 .or. maxval(mNST).lt.2)
     +   STOP 'AllocArrays: bad HSS/NST'
      GB = (44.5d0*dble(maxval(mNST))**2
     +      + 30.d0*maxval(mNST)*nXE*LaMx)*8.d0/1.d9
      write(*,'(a,i6,a,i4,a,f6.2,a)') ' Params0.inp: nQSm = max(NST) =',
     +   maxval(mNST), ',  HSSm = max(HSS) =', maxval(mHSS),
     +   ';  arrays ~', GB, ' GB'
      CALL AllocSized(maxval(mNST), maxval(mHSS))
      END SUBROUTINE AllocArrays


      SUBROUTINE AllocSized(nQSmIn, HSSmIn)  ! allocate and zero all arrays of
      integer nQSmIn, HSSmIn                 ! size nQSm, HSSm (also used by tests)
      nQSm = nQSmIn
      HSSm = HSSmIn
      Nwork= 50+ 12*nQSm+ nQSm**2

      allocate(QSname(nQSm,nXE))
      QSname = ' '
      allocate(nuAS(HSSm,nXE), nuQS(HSSm,nXE), nuGS(HSSm+1,nXE),
     +   kAI1(HSSm,nXE), kAI2(HSSm,nXE), kiSS(nQSm,nXE), pqn(nQSm,nXE),
     +   nu1(nQSm), nu2(nQSm), eqEL(nQSm,nXE), orb(nQSm,nXE),
     +   MthdEX(nQSm,nQSm))
      nuAS= 0;  nuQS= 0;  nuGS= 0;  kAI1= 0;  kAI2= 0;  kiSS= 0;  pqn= 0
      nu1= 0;   nu2= 0;   eqEL= 0;  orb= 0;   MthdEX= 0

      allocate(LvJW(LaMx,nXE,nQSm), LvLorW(LaMx,nXE,nQSm),
     +   PI(HSSm,nXE), PIR(HSSm,nXE,LaMx), DPI(HSSm,nXE,LaMx),
     +   E(nQSm,nXE), g0(nQSm,nXE), BE(nQSm,nXE,LaMx),
     +   BEp(nQSm,nXE,LaMx), bra(nQSm,nQSm,nXE), A(nQSm,nQSm,nXE),
     +   WAiz(nQSm,nQSm,nXE), flu(nQSm,nQSm,nXE))
      LvJW= zero;  LvLorW= zero;  PI= zero;   PIR= zero;  DPI= zero
      E= zero;     g0= zero;      BE= zero;   BEp= zero;  bra= zero
      A= zero;     WAiz= zero;    flu= zero

      allocate(Ax(nQSm,nQSm), Bx(nQSm,nQSm), Cx(nQSm,nQSm),
     +   Dx(nQSm,nQSm), Ex(nQSm,nQSm), Fx(nQSm,nQSm),
     +   Aix(nQSm,nQSm), Bix(nQSm,nQSm), Cix(nQSm,nQSm), Dix(nQSm,nQSm),
     +   Eix(nQSm,nQSm), Fix(nQSm,nQSm), Gix(nQSm,nQSm), Hix(nQSm,nQSm),
     +   Eth(nQSm,nQSm))
      Ax= zero;  Bx= zero;  Cx= zero;  Dx= zero;  Ex= zero;  Fx= zero
      Aix= zero; Bix= zero; Cix= zero; Dix= zero
      Eix= zero; Fix= zero; Gix= zero; Hix= zero; Eth= zero

      allocate(WI(nQSm,nQSm), WEX(nQSm,nQSm), WDX(nQSm,nQSm),
     +   WRR(nQSm,nQSm), WTB(nQSm,nQSm), WInd(nQSm,nQSm),
     +   Wab(nQSm,nQSm), WPhI(nQSm,nQSm), WiRR(nQSm,nQSm),
     +   WDC(nQSm,nQSm), EAI(nQSm,nQSm), PM(nQSm,nQSm))
      WI= zero;   WEX= zero;  WDX= zero;  WRR= zero;  WTB= zero
      WInd= zero; Wab= zero;  WPhI= zero; WiRR= zero; WDC= zero
      EAI= zero;  PM= zero

      allocate(Wout(nQSm), WoutXE(nQSm,nXE,LaMx), POP(nQSm),
     +   POPi(nQSm,nXE,LaMx), POPf(nQSm,nXE,LaMx),
     +   POPZ(HSSm+1,nXE,LaMx), WEAF(Nwork))
      Wout= zero;  WoutXE= zero;  POP= zero;  POPi= zero;  POPf= zero
      POPZ= zero;  WEAF= zero
      END SUBROUTINE AllocSized


      SUBROUTINE AllocLines(nLines)  ! Generic version: MNLe = number
      integer nLines                 ! of spectral lines + 1 (LineList)
      MNLe = nLines + 1
      allocate(nX3(MNLe), nUp(MNLe), nLo(MNLe),
     +   nUph(MNLe,nXE), nLoh(MNLe,nXE),
     +   Norm(MNLe,LaMx), hvCh(MNLe,nXE), hvC(MNLe),
     +   QMlimit(MNLe,LaMx), FWevLor(MNLe,LaMx))
      nX3= 0;  nUp= 0;  nLo= 0;  nUph= 0;  nLoh= 0
      Norm= zero;  hvCh= zero;  hvC= zero;  QMlimit= zero
      FWevLor= zero
      END SUBROUTINE AllocLines

      end module	  


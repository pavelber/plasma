      module mo1code2  
      implicit none
      integer, public, parameter :: 
     +     nXE= 4              ! number of chemical elements (XE) in present simulation: 
c                                nX=1,2,3,4 is Kr,C,He,D along ion mass, see Params0.inp. 
      integer, public :: 
     +     FSS(nXE), HSS(nXE), ! First & H-like Spectroscopic Symbol (SpS) in database (DaBa); FSS can be any, but it must have > 1 energy levels (ELs)
     +     Nnu(nXE),           ! nuclei order # in databases = the number of "non-AI" Energy Levels (ELs) 
     +     NST(nXE)            ! number of ELs in each of 4 DaBa [non-AI and AI] : .

      integer, public, parameter :: 
     +     nQSm = 1650,                 ! number of ELs in X (nX=1). For HSSm=36 it must be <= 2300 but for HSSm>36 integer nQSm must be < 2300, to be found in compilation	   
     +     HSSm = 36,                   ! max SpS of H-like ion in DaBa; [36 for Kr]
     +    Nwork = 50+ 12*nQSm+ nQSm**2  ! For NAG d02eaf workArea 'WEAF'	
      integer, public, parameter :: 
     +   nvL  = 30000,  ! Reserved number of hv-grid points. True number of v-points "nvM" <= "nvL" in defined in "vGrid" subr"
     +   MNLe = 16000,  ! Not more than "MNLe" spectral lines are expected for all XE together (common LineList)
     +   LaMx = 3,      ! Number of types of zones
     +   mSpe = 8,      ! Number of frames.
     +   ntp  = mSpe+1  ! Number of t-points in Scenario   

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
     +   sqpi = 1.77245385d0,   ! sqrt(pin)
     +   hBar = hPL/ToPi,       ! eV*s;  Planck bar        
     +   eVA  = 12398.424d0,    ! A*eV;  hv(eV)*Lambd(A)
     +   Ampli= 2.362d-13,      ! 8*pi/sqrt(3) * pi*ao^2 * Ry^2  for VR       
     +   zero = 0.d0,
     +   one  = 1.d0,
     +   two  = 2.d0,			                          
     +  third = one/3.d0,
     +   CoSp = 3.d0/FoPi, ! for volume of sphere
     +   tol  = 1.d-3,     ! tolerance in NAG d01;  must be <= 10^-7. Checked by using equal GT for cells #1 & #3 in z1518 with RF=0
     +  tolD02= 1.d-7,     ! tolerance in NAG d02;  NOTE: at some t-step it gave "SumPOP-1 > d-8", thus I allowed "SumPOP-1" up to d-7, see "My STOP after d02"  
     +   MuAU = Uam/elma   ! proton mass in e-mass units, i.e. in AU        

      character*24, public :: QSname(nQSm,nXE)     ! part 1, according to 'Inz.inp' list
      character*9,  public :: title, empty, comme  ! in input files reading

      integer, public :: 
     +  nuAS(HSSm,nXE),   ! Number of AutoIonizing (AI) ELs in each SpS
     +  nuQS(HSSm,nXE),   ! Number of non-AI ELs in each SpS via "QSs.inp"
     +  nuGS(HSSm+1,nXE),                ! EL# of ground state ("GrSt")'s of each SpS, including nucl.
     +  kAI1(HSSm,nXE), kAI2(HSSm,nXE),  ! for AI ELs of each SpS, their first & last # in total list of ELs
     +  kiSS(nQSm,nXE),  pqn(nQSm,nXE),  ! SpS of each EL & XE;  For each EL: principal quantum number of outer shell
     +  nu1(nQSm), nu2(nQSm),            ! Level order # in Z-stage, Level order ## in total list
     +  eqEL(nQSm,nXE),  orb(nQSm,nXE),  ! number of equivalent electrons in nl-subshell; orbital q.n.
     +  MthdEX(nQSm,nQSm),               ! formula # for fitting e-impact excitation cross-section of high-Z dopant 
     +  nX3(MNLe), nUp(MNLe), nLo(MNLe), ! XE and EL serial numbers of Upper & Lower levels of spectral line after ordering the lines over hv
     +  nUph(MNLe,nXE), nLoh(MNLe,nXE),  ! same prior to the ordering
     +  LastAI(nXE),                     ! EL-List number of the last AI EL;  calculated in "Intro"
     +  linM(nXE),    ! number of term-to-term lines in [hvmin, hvmax] domain, except those to be FLAG-replaced by their FS components
     +  AskInf(100),  ! Kr EL numbers for printing PM-info in file (49                  
     +  linM4,        ! number of spectral lines in [hvmin, hvmax] domain (sum over XE) 
     +  La,    ! serial # of plasma layer under study (w-layer)
     +  nX,    ! variable (current) number of Chemical Element (XE)
     +  mePh,          
     +  nvMF, nvM,       ! number of hv-points in full interval, see subr "hvPoints"   
     +  k, kf, j, jx, i,                       ! variable integers
     +  iv, kw, k7, k1, k2, iSS, jSS, kQS, kS, ! variable integers
     +  kU, kL, lin, nLam, nXw, n7, lw,        ! variable integers
     +  npts, Nlim, Ifail,                     ! integers in D01ahf and d02
     +  nC, nw,            ! # of central point of r-grid for vertical (radial) scans 
     +  iX,      
     +  nuBSs,   ! number of BSs in the core  
     +  nqInf,   ! for how many ELs you want detailed PM-info in file (49	?
     +  LaInf,   ! cell# of this info
     +  nFine,
     +  Count, StrExc, CountExc, StrInz, CountInz, 
     +  StrAIw, CountAIw, iw

      real(8), public :: 
     +   EmTot(LaMx, nvL),        ! plasma emissivity [W/cc/sr/eV] in LaMx shells 
     +   AbTot(LaMx, nvL),        ! plasma absorption coefficient corrected for stimulated emission [1/cm] in the same shells
     +   absoFF(nvL), emisFF(nvL),  
     +   absoBF(nvL), emisFB(nvL),  
     +   absoBB(nvL), emisBB(nvL),  
     +     LvJW(LaMx, nXE, nQSm),  ! LEVEL Stark width [eV], J-formulary 
     +   LvLorW(LaMx, nXE, nQSm),  ! same corrected for uncertainty principle
     +    PI(HSSm,nXE),            ! Ionization Potential of each GS, table value
     +   PIR(HSSm,nXE,LaMx),  ! Ionization Potential of each GS, reduced by continuum lowering
     +   DPI(HSSm,nXE,LaMx),  ! Reduction of Ionization Potential according to 'KeRedu'
     +  Price(nQSm,nXE,LaMx), ! EL energy relative to E(1). Commonly GS ATOM has E=0, but it may have E>0 if term; "Price" accounts for continuum lowering
     +    E(nQSm,nXE),        ! EL Energy relative to THIS-SS ground state
     +   g0(nQSm,nXE),        ! EL degeneracy: table value
     +    BE(nQSm,nXE,LaMx),  ! Binding Energy of each EL 'ti'-corrected in 'AtKins' after continuum lowering
     +   BEp(nQSm,nXE,LaMx),  ! after 'call AtKins', 'tf'-Binding Energy of w-layer is saved until next visit to this 'La'; then previous-t BE may be needed for restart of dead EL.
     +   bra(nQSm,nQSm,nXE),  ! with FAC bases, we assign bra= 1/0 based on yes/no single-e ioniz cross-Sec for this i/f couple in "InzXE....inp" file.  
     +     A(nQSm,nQSm,nXE),  ! Einstein A coef [1/s]
     +  WAiz(nQSm,nQSm,nXE),  ! AutoIoniz Probability [1/s]
     +   flu(nQSm,nQSm,nXE),  ! Correct-sign (>0) Absorption Oscillator Strength
     +    Ax(nQSm,nQSm),      ! Ax-Fx are 6 coefs used in fitting formulae for 
     +    Bx(nQSm,nQSm),      ! e-impact excitation cross-sections of 
     +    Cx(nQSm,nQSm),      ! high-Z dopant, see Exc.inp 
     +    Dx(nQSm,nQSm),
     +    Ex(nQSm,nQSm), 
     +   Aix(nQSm,nQSm),  ! Aix-Dix are 4 coefs of e-impact ioniz cross-section of high-Z dopant, see "Inz.inp"
     +   Bix(nQSm,nQSm),  
     +   Cix(nQSm,nQSm), 
     +   Dix(nQSm,nQSm),
     +   Eix(nQSm,nQSm),  ! Eix-Hix are 4 coefs of photo-ioniz cross-section of high-Z dopant, see "Inz.inp"
     +   Fix(nQSm,nQSm), 
     +   Gix(nQSm,nQSm), 
     +   Hix(nQSm,nQSm), 
     +   Eth(nQSm,nQSm),  ! State-to-state Ionization Threshold (for ioniz cross-secs) 
     +   SpInEf(LaMx,nvL),  ! OMEGA-mean RF intensity in 3 shells [W/cm2/sr/eV]
     +   SpeP(LaMx,nvL),    ! Spectral power of radiation from single BS & from DT core [W/eV] 
     +   SpePowOut(nvL),  ! same in 1D array for Gau-convo subr           
     +   SpePowConv(nvL), ! Same instr-Conv
     +   SpeY(nvL),       ! t-depe spectral yield [J/keV/sr] gathered since t0 till "StopTime" 
     +    WI(nQSm,nQSm),                   ! single ionization probability (1/s), removal of one electron per e-impact
     +   WEX(nQSm,nQSm),  WDX(nQSm,nQSm),  ! e-impact excitation & deexcitation probabilities (1/s)
     +   WRR(nQSm,nQSm),  WTB(nQSm,nQSm),  ! radiative & 3-body recombination probabilities (1/s)
     +  WInd(nQSm,nQSm),  Wab(nQSm,nQSm),  ! induced emission & Photo(BB)absorption probabs (1/s)
     +  WPhI(nQSm,nQSm), WiRR(nQSm,nQSm),  ! photoIonization & induced RR probabs, 1/s
     +   WDC(nQSm,nQSm),                   ! probability of Dielectronic Capture into AI EL of He-like & Li-like ions
     +   EAI(nQSm,nQSm),   ! FACn energy of AI transition (column "DE[eV]" in "AIw.inp)"    
     +    PM(nQSm,nQSm),   ! Probability Mtrx [1/s], t-Scale=10^40 for d02
     +  Wout(nQSm),             ! EL depletion rate, PM(k,k)= -Wout(k)  
     +  WoutXE(nQSm,nXE,LaMx),  ! "Wout" remembered for each XE for Stark
     +  POP(nQSm),              ! relative abundance of QSs
     +  POPi(nQSm, nXE, LaMx),  ! ti-values of EL POP saved La,nX-dependent 
     +  POPf(nQSm, nXE, LaMx),  ! save as 'tf" value for loading in POPs in Scenario for next ti
     +  POPZ(HSSm+1,nXE,LaMx),  ! relative abundance of ionization stages, including nucl, which has SS= HSS+1 
     +  HoF(nXE+1),             ! Holtzmark Fo-field [a.u.] due to i of one XE or electrons, J (3.49) 
     +  niFra(nXE,LaMx),        ! fraction of XE in total ion number density DenI(La) [i/cc]
     +  WEAF(Nwork),      ! NAG params and WorkArea 
     +   FrP(mSpe), ! time [s] of centers of "mSpe" frames  
     +   tPo(ntp),  ! t-points including t=0 and 1 point after last spectra printout
     +   R1t(ntp),  ! radius [cm] of BSs in the t-point #tip
     +   R2t(ntp),  ! radius of Core    [cm] in the t-point #tip
     +   R3t(ntp),  ! radius of Capsule [cm] in the t-point #tip
     + nuBSst(ntp), ! number of BSs in the core  
     +   W1D(ntp),            ! w-array for reading strings of Params2.inp
     +   nit(ntp, LaMx, nXE), ! t-points of ion number density of each XE [i/cc] in La
     +   Tet(ntp, LaMx),      ! t-points of Te [eV] in La
     +  u3Dt(ntp, LaMx),      ! t-points of u3D [cm/s] in La
     +   bpt(ntp, LaMx),      ! t-points of e-beam part of EED in La
     +   bct(ntp, LaMx),      ! t-points of e-beam center [eV] in e-beam part of EED in La
     +   bwt(ntp, LaMx),      ! t-points of e-beam width  [eV] in e-beam part of EED in La
     +  FrYie(nvL,mSpe), ! array of 8 frames [J/keV/sr] 
     +   ceR(LaMx),      ! [cm] outer radius of spherical zone            
     +   u3D(LaMx),   ! mean absolute velocity of ions in their isotropic collective (hydro) 3D motion in zone number La.
     +   Te(LaMx),    ! electron  temperature in La-layer [eV], prescribed in Scenario
     +   Tion(LaMx),  ! ion temperature 
     +   Den(nXE,LaMx), ! ion number density [i/cc] of all XE in each "La"   
     +   DenI(LaMx),    ! total (all-XE) ion density, i/cc  
     +   Dene(LaMx),    ! electron density, e/cc
     +   ZC(nXE,LaMx),            ! each-XE mean ion charge  
     +   ZC1(LaMx), ZC1pr(LaMx),  ! mean (all-XE, all-SS, inclu atoms) ion charge 
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
     +   Norm(MNLe,LaMx),           ! integral of the line shape: it becomes < 1 after hard wings cut & must be used for re-norm of the shapes to 1 
     +   hvCh(MNLe,nXE), hvC(MNLe), ! Spectral line Centers before & after hv-regulation
     +   QMlimit(MNLe,LaMx),        ! lower limit for level width from uncertainty princip   
     +   fwTiKvC(LaMx,nXE), 
     +   fwTUKvC(LaMx,nXE),   ! for-broadening info
     +   FWkVcGAU(LaMx,nXE),  ! FWHMgau/hvC via u3D, same for all lines of XE. 
     +   FWevLor(MNLe,LaMx),  ! Lorentzian (Stark) part of FWHM [eV]  
     +   Xc(LaMx), uC(LaMx),  ! X-coord of the La-center & plasma flow velocity re-calculated to the cell center 
     +   hvV(nvL),                 ! hv-grid [eV] & current value at any grid point: hveV= hvV(iv)
     +   Yield(nvL), YieConv(nvL),
     +   dis(mSpe),                ! distance to periph LOS in TREX scans
     +   DiZ2(LaMx),                             ! XE-sum of Den*Z^2 for FF emission
     +   Zdot(LaMx), ZdotPr(LaMx), ZdotN(LaMx),  ! XE-mean dZ/dt; revious
     +   dZpdt(nXE),                             ! XE-resolved dZ/dt
     +   bp(LaMx), bc(LaMx), bw(LaMx),  ! params of e-beam for non=Maxw EED (given by "bp > 0."), see EED function 
     +   tiInf,                         ! time to display "...LineInfo.dat", "...EmiAbso.dat", "EffSpIns.dat"  
     +   wBS, ScaF,  
     +   hvMin, hvMax, hveV, ! edges of the v-grid [eV] from FLAG
     +   RadPow, RadPowFF,   ! outgoing Radiation Power thru plasma surf & FF part of this RadPower, [W]
     +   totSt,              ! A.U., LEVEL Stark width due to all e,i 
     +   wwr, tiS, tfS,  ! work ratio
     +   TeAU, TiAU,     ! Te, Ti in A.U. = hartree = 2*Ry = 27.211396 eV
     +   DeAU,       ! electron density in "e per a0^3", i.e. ne [e/cc] * 1.48185e-25 cc 
     +   Amass,      ! ion mass [g],   Amass= AtMass(nX)*1.66054d-24 
     +   AulMin,     ! Min allowed value of Einst A() coef. If A() < AulMin, we replace it by A() =0 in "Intro" 
     +   fluMin,     ! Min allowed value of Abso OscStrenght. If f() < fluMin, we replace A bt 0 in "Intro" 
     +   DE, BEk,    ! E betw two levels under consideration, E required for any 'k'-->'kf' transition, init energy of free el in fb trans. 
     +   stopTime,          ! time to stop the run [s].  Instant chosen for CRE calculations, both from FLAG 
     +   SumP, Scale, Sca2, ! sum POP 
     +   BroIns,            ! instrumental broadening of spectral lines; == vC/FWHMinstr for thin line
     +   tstep, ti, tf,   ! time-step [s]. Initial & final instants of present time-step: tf= ti+ tstep; tm=(ti+tf)/2. Start-value of "ti".
     +   strt, tm, rer,   ! used in D01ahf(...)
     +   TeDotFrE,        ! part of 'dTe/dt', namely, what is due to thermalization of fresh electrons to 'Te'
     +   Zw, wtot, wtot1, ! w-var for Zbar and CPpCm in 'AtKins' 
     +   POPk, POPkr,     ! used for PhI with  
     +   rPSI, distII,    ! used in functions SigExc, SigInz, SigPhi for avoiding too large values via FAC fit errors.  No corre in REVERSE processes as (1) they are expre via direct & (2) must provide equilibration.             
     +   upTB, Incr, frac,  
     +   LambA, hvAxiRe,  ! the finest resolution (dv/hvC) on hv axis  
     +   ReduReso, ReReiv,    
     +   distBS, hvFine,     ! hv at which spectral resolution is the finest
     +   Asum, R1, R2, R3,   ! more practical than "CeR(La)"  
     +   AsumU, AsumL,  
     +   hvPrint1, hvPrint2, ! [keV] edges of Print-Out interval for "SideOnSpIn.dat"  
     +   uTi, nDE,           ! [cm/s] mean thermal velo of ion
     +   FrL, Aper,              ! duration of x-ray frame   
     +   pathBS, pathCO, pathCA,
     +   Wing, dhv, SpInPL, TPL, 
     +   MaxExch, POPexch, 
     +   hvKey, WingCut,         ! restricts hv-length of line wing via Wing = hvC(line)*WingCut      
     +   hvIns1, hvIns2, A1ins, B1ins, A2ins, B2ins, A3ins, B3ins
      end module	  


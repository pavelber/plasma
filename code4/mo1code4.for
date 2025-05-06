      module mo1code4  
      implicit none
      integer, public, parameter :: 
     +     nXE= 4              ! number of chemical elements (XE) in present simulation: 
c                                nX=1,2,3,4 is Fe, Cr, Ni, Mn 
      integer, public :: 
     +     FSS(nXE), HSS(nXE), ! First & H-like Spectroscopic Symbol (SpS) in database (DaBa); FSS can be any, but it must have > 1 energy levels (ELs)
     +     Nnu(nXE),           ! nuclei order # in databases = the number of "non-AI" Energy Levels (ELs) 
     +     NST(nXE)            ! number of ELs in each of 4 DaBa [non-AI and AI] : .

      integer, public, parameter :: 
     +   nQSm = 387,                  ! number of ELs in X (nX=1). For HSSm=36 it must be <= 2300 but for HSSm>36 integer nQSm must be < 2300, to be found in compilation	   
     +   HSSm = 28,                   ! max SpS of H-like ion in DaBa; [36 for Kr]
     +   Nwork = 50+ 12*nQSm+ nQSm**2 ! For NAG d02eaf workArea 'WEAF'	
      integer, public, parameter :: 
     +   nvL  = 30000,  ! Reserved number of hv-grid points. True number of v-points "nvM" <= "nvL" in defined in "vGrid" subr"
     +   MNLe = 20000,  ! Not more than "MNLe" spectral lines are expected for all XE together (common LineList)
     +   LaMx = 2,      ! Number of zones	(here Core and Halo)
     +   mSpe = 8,      ! Number of frames.
     +   ntp  = mSpe+2, ! Number of t-points in Scenario   
     +   np5kap= 2272,  ! Number of hv-points on response curve of PCD with  5 mils kapton filter  [1mil = 0.001 inch = 25.4 mcm] 
     +   np10ka= 2301,  ! Number of hv-points on response curve of PCD with 10 mils kapton filter  
     +   np40ka= 2194   ! Number of hv-points on response curve of PCD with 40 mils kapton filter  

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

      character*9, public :: QSname(nQSm,nXE)     
      character*9, public :: title, empty, comme  ! in input files reading

      integer, public :: 
     +  nuAS(HSSm,nXE),   ! Number of AutoIonizing (AI) ELs in each SpS
     +  nuQS(HSSm,nXE),   ! Number of non-AI ELs in each SpS via "QSs.inp"
     +  nuGS(HSSm+1,nXE),                ! EL# of ground state ("GrSt")'s of each SpS, including nucl.
     +  kAI1(HSSm,nXE), kAI2(HSSm,nXE),  ! for AI ELs of each SpS, their first & last # in total list of ELs
     +  kiSS(nQSm,nXE),  pqn(nQSm,nXE),  ! SpS of each EL, each XE;  For each EL: principal quantum number of outer shell
     +  eqEL(nQSm,nXE),  orb(nQSm,nXE),  ! number of equivalent electrons in nl-subshell; orbital q.n.
     +  MthdEX(nQSm,nQSm, nXE),          ! formula # for fitting e-impact excitation cross-section 
     +  nX3(MNLe), nUp(MNLe), nLo(MNLe), ! XE and EL serial numbers of Upper & Lower levels of spectral line after ordering the lines over hv
     +  nUph(MNLe,nXE), nLoh(MNLe,nXE),  ! same prior to the ordering
     +  LastAI(nXE),                     ! EL-List number of the last AI EL;  calculated in "Intro"
     +  linM(nXE),    ! number of term-to-term lines in [hvmin, hvmax] domain, except those to be FLAG-replaced by their FS components
     +  AskInf(100),  ! Kr EL numbers for printing PM-info in file (49                  
     +  linM4,        ! number of spectral lines in [hvmin, hvmax] domain (sum over XE) 
     +  La,     ! serial # of plasma layer under study (w-layer)
     +  nX,     ! variable integer shows serial number of Chemical Element (XE)
     +  LastFr, ! number of frames before "StopTime" 
     +  mePh,       ! formula number for  photoionization cross-sections          
     +  nvMF, nvM,  ! number of hv-points in full interval, see subr "hvPoints"   

     +  k, kf, j, jx, i, iw, kfr, kfrp, CoFr,  ! variable integers
     +  iv, kw, k7, k1, k2, iSS, jSS, kQS, kS, ! variable integers
     +  kU, kL, lin, nLam, nXw, n7, lw, iX,    ! variable integers
     +  npts, Nlim, Ifail                      ! integers in D01ahf and d02

      real(8), public :: 
     +   hvV(nvL),                ! hv-grid [eV] 
     +   EmTot(LaMx, nvL),        ! plasma emissivity [W/cc/sr/eV] in LaMx shells 
     +   AbTot(LaMx, nvL),        ! plasma absorption coefficient corrected for stimulated emission [1/cm] in the same shells
     +   absoFF(nvL), emisFF(nvL),  
     +   absoBF(nvL), emisFB(nvL),  
     +   absoBB(nvL), emisBB(nvL), 
     +   SpInEf(LaMx,nvL),        ! OMEGA-mean RF intensity in 2 zones [W/cm2/sr/eV]
     +   RadPow(nvL),     ! [W/eV/sr) in side-on direction  
     +   SpeY(nvL),         ! spectral yield [J/keV/sr] gathered since t0 till current "tf" 

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
     +    Ax(nQSm,nQSm,nXE),  ! Ax-Ex are 5 coefs used in fitting formulae for 
     +    Bx(nQSm,nQSm,nXE),  ! e-impact excitation cross-sections  
     +    Cx(nQSm,nQSm,nXE),       
     +    Dx(nQSm,nQSm,nXE),
     +    Ex(nQSm,nQSm,nXE), 
     +   Aix(nQSm,nQSm,nXE),  ! Aix-Dix are 4 coefs of e-impact ioniz cross-section
     +   Bix(nQSm,nQSm,nXE),  
     +   Cix(nQSm,nQSm,nXE), 
     +   Dix(nQSm,nQSm,nXE),
     +   Eix(nQSm,nQSm,nXE),  ! Eix-Hix are 4 coefs of photo-ioniz cross-section
     +   Fix(nQSm,nQSm,nXE), 
     +   Gix(nQSm,nQSm,nXE), 
     +   Hix(nQSm,nQSm,nXE), 
     +   Eth(nQSm,nQSm,nXE),  ! State-to-state Ionization Threshold (for ioniz cross-secs) 
     +   EAI(nQSm,nQSm,nXE),  ! From-FAC energy of AI transition before lowering of continuum (column "DE[eV]" in "AIwXE.inp)"    

     +    WI(nQSm,nQSm),                   ! single ionization probability (1/s), removal of one electron per e-impact
     +   WEX(nQSm,nQSm),  WDX(nQSm,nQSm),  ! e-impact excitation & deexcitation probabilities (1/s)
     +   WRR(nQSm,nQSm),  WTB(nQSm,nQSm),  ! radiative & 3-body recombination probabilities (1/s)
     +  WInd(nQSm,nQSm),  Wab(nQSm,nQSm),  ! induced emission & Photo(BB)absorption probabs (1/s)
     +  WPhI(nQSm,nQSm), WiRR(nQSm,nQSm),  ! photoIonization & induced RR probabs, 1/s
     +   WDC(nQSm,nQSm),                   ! probability of Dielectronic Capture into AI EL of He-like & Li-like ions
     +    PM(nQSm,nQSm),   ! Probability Mtrx [1/s], t-Scale=10^40 for d02
     +  Wout(nQSm),             ! EL depletion rate, PM(k,k)= -Wout(k)  
     +  WoutXE(nQSm,nXE,LaMx),  ! "Wout" remembered for each XE for Stark
     +  POP(nQSm),              ! relative abundance of QSs
     +  POPi(nQSm, nXE, LaMx),  ! ti-values of EL POP saved La,nX-dependent 
     +  POPf(nQSm, nXE, LaMx),  ! save as 'tf" value for loading in POPs in Scenario for next ti
     +  POPZ(HSSm+1,nXE,LaMx),  ! relative abundance of ionization stages, including nucl, which has SS= HSS+1 
     +  HoF(nXE+1),             ! Holtzmark Fo-field [a.u.] due to i of one XE or electrons, J (3.49) 
     +  niFra(nXE,LaMx),        ! fraction of XE in total ion number density DenI(La) [i/cc]
     +  WEAF(Nwork),     ! NAG params and WorkArea 

     +        FrP(mSpe), ! time [s] of centers of "mSpe" frames  
     +  FrYie(nvL,mSpe), ! array of 8 frames [J/keV/sr] 

     +   tPo(ntp), ! t-points including 1 t-point before 1st frame and 1 t-point after last frame
     +   R1t(ntp), ! radius [cm] of Core at t-points given in Params2.inp
     +   R2t(ntp), ! radius [cm] of Halo ................
     +   LZt(ntp), ! Length [cm] of zones ................  
     +   W1D(ntp),        ! w-array for reading strings of Params2.inp
     +   nit(ntp, LaMx),  ! t-points of ion number density of each XE [i/cc] in La
     +   Tet(ntp, LaMx),  ! t-points of Te [eV] in La
     +  u3Dt(ntp, LaMx),  ! t-points of u3D [cm/s] in La
     +   bpt(ntp, LaMx),  ! t-points of e-beam part of EED in La
     +   bct(ntp, LaMx),  ! t-points of e-beam center [eV] in e-beam part of EED in La
     +   bwt(ntp, LaMx),  ! t-points of e-beam width  [eV] in e-beam part of EED in La

     +   ceR(LaMx),   ! [cm] outer radii of cylindrical zones            
     +   u3D(LaMx),   ! mean absolute velocity of ions in their isotropic collective (hydro) 3D motion in zone number La.
     +    Te(LaMx),   ! electron  temperature in La-layer [eV], prescribed in Scenario
     +   Tion(LaMx),  ! ion temperature 
     +   Den(nXE,LaMx), ! ion number density [i/cc] of all XE in each "La"   
     +   DenI(LaMx),    ! total (all-XE) ion density, i/cc  
     +   Dene(LaMx),    ! electron density, e/cc
     +   ZC(nXE,LaMx),            ! each-XE mean ion charge  
     +   ZC1(LaMx), ZC1pr(LaMx),  ! mean (all-XE, all-SS, inclu atoms) ion charge 

     +   hv5kap(np5kap), Resp5kap(np5kap),  ! in input files: hv points of response of PCD with  5 mils kapton filter  [1mil = 0.001 inch = 25.4 mcm] 
     +   hv10ka(np10ka), Resp10ka(np10ka),  !                 hv points of response of PCD with 10 mils kapton filter. Remember 0 <= Respo <=1 
     +   hv40ka(np40ka), Resp40ka(np40ka),       !            hv points of response of PCD with 40 mils kapton filter. 
     +   Resp40(nvL), Resp10(nvL), Respo5(nvL),  ! at hv points of present computation: Response of three PCDs. 0 <= Respo <=1  

     +   WSr(nXE+1), ! Wigner-Seitz radius for ions of same XE or electrons
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
     +   FWkVcGAU(LaMx,nXE),  ! FWHMgau/hvC via u3D, same for all lines of XE. 
     +   FWevLor(MNLe,LaMx),  ! Lorentzian (Stark) part of FWHM [eV]  
     +   Xc(LaMx), uC(LaMx),  ! X-coord of the La-center & plasma flow velocity re-calculated to the cell center 
     +   DiZ2(LaMx),                    ! XE-sum of Den*Z^2 for FF emission
     +   bp(LaMx), bc(LaMx), bw(LaMx),  ! params of e-beam for non=Maxw EED (given by "bp > 0."), see EED function 
     +   tiInf,                         ! time to display "...LineInfo.dat", "...EmiAbso.dat", "EffSpIns.dat"  
     +   hvMin, hvMax, hveV, ! edges of the v-grid [eV] from FLAG
     +   totSt,              ! A.U., LEVEL Stark width due to all e,i 
     +   wwr, tiS, tfS,  ! work ratio
     +   TeAU, TiAU,     ! Te, Ti in A.U. = hartree = 2*Ry = 27.211396 eV
     +   DeAU,       ! electron density in "e per a0^3", i.e. ne [e/cc] * 1.48185e-25 cc 
     +   AulMin,     ! Min allowed value of Einst A() coef. If A() < AulMin, we replace it by A() =0 in "Intro" 
     +   fluMin,     ! Min allowed value of Abso OscStrenght. If f() < fluMin, we replace A bt 0 in "Intro" 
     +   DE, BEk,    ! E betw two levels under consideration, E required for any 'k'-->'kf' transition, init energy of free el in fb trans. 
     +   stopTime,    ! time to stop the run [s].  Instant chosen for CRE calculations, both from FLAG 
     +   SumP, Scale, ! sum POP 
     +   BroIns,        ! instrumental broadening of spectral lines; == vC/FWHMinstr for thin line
     +   tstep, ti, tf,   ! time-step [s]. Initial & final instants of present time-step: tf= ti+ tstep; tm=(ti+tf)/2. Start-value of "ti".
     +   strt, tm, rer,   ! used in D01ahf(...)
	+   AngPCD,AngTREX,	! Angle between z-pinch axis and directions to PCDs and TREX

     +   Zw, wtot, wtot1, ! w-var for Zbar and CPpCm in 'AtKins' 
     +   POPk, POPkr,     ! used for PhI with  
     +   rPSI, distII,    ! used in functions SigExc, SigInz, SigPhi for avoiding too large values via FAC fit errors.  No corre in REVERSE processes as (1) they are expre via direct & (2) must provide equilibration.             
     +   upTB, Incr, frac,  
     +   LambA, hvReso,   ! the finest resolution (dv/hvC) on hv axis  
     +   ReduReso, ReReiv,    
     +   hvFine,          ! hv at which spectral resolution is the finest
     +   R1, R2, Lz,      ! more practical than "CeR(La)"  
     +   Asum, AsumU, AsumL,  
     +   hvPrint1, hvPrint2, ! [eV] edges of Print-Out interval for "SideOnSpIn.dat"  
     +   nDE, FrL,           ! duration of x-ray frame; 
     +   pathCO, pathCA,
     +   Wing, dhv, SpInPL, TPL, 
     +   MaxExch, POPexch, 
     +   hvKey, WingCut,     ! restricts hv-length of line wing via Wing = hvC(line)*WingCut      
     +   hvIns1, hvIns2, Ains, Bins, Cins
      end module	  


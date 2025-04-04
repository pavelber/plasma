      module mo1     
      implicit none
      integer, public, parameter :: 
     +     nXE= 2              ! number of chemical elements (XE). Now nX=1 is AL, nX=2 is MG. 
      integer, public :: 
     +     FSS(nXE), HSS(nXE), ! First & H-like Spectroscopic Symbol (SpS) in database(DB); "FSS" may be any, but it must have > 1 energy level (EL).
     +     Nnu(nXE),           ! Number of non-autoionizing energy levels (ELs) in DB. "Nnu" = the serial number of nucleus in DB
     +     NST(nXE)            ! TOTAL number of ELs in DB of XE, This number includes autoionizing (AI) and non-AI ELs 

      DATA FSS / 10,  9/,   ! 1st SpSs of AL & MG in DB, here Be-like ELs
     +     HSS / 13, 12/,   ! H -like SpSs
     +     Nnu /109, 109/,  ! serial number of nuclei in DBs of XE. This number = the number of non-AI ELs in DB 
     +     NST /272, 272/   ! total number of ELs in databases of Al and Mg  
      integer, public, parameter :: 
     +     NSTm = 272,            ! max of all (here two) "NST"	   
     +     HSSm =  13,                  ! highest (of all XE) SpS of H-like ion
     +    Nwork = 50+ 12*NSTm+ NSTm**2  ! For NAG d02eaf work area 'WEAF'	

      integer, public, parameter :: 
     +   nFAI = 201,   ! in each SpS, containing AI ELs: artificial number for the 1st (lowest) AI EL on the list of this SpS .  
     +   nvM  = 12300, ! number of hv points on hv axis. Among them: 100 points in soft [hvMin, hvSmo] domain & 200 points in [hvMaxf, hvMax] domain 
     +   mSpe = 6,     ! Number of TREX frames = number of t-points chosen for printing files 61 - 76,171-176, 371-376,
c                        = number of t-points in scenario.      
     +   MNLe = 2600,  ! Not more than "MNLe" spectral lines are expected for all XEs in common LineList
     +   LaMx = 2,     ! Number of zones, here 1 sphere but it is processed as La=2
     +   np5kap= 2272, ! Number of v-points on  5 mils kapton PCD response curve [1mil = 25.4 mcm; mils= mInch] 
     +   np10ka= 2301, ! Number of v-points on 10 mils kapton PCD  
     +   np40ka= 2194, ! Number of v-points on 40 mils kapton PCD  
     +   npBe8 = 2455, ! Number of v-points on 8 mcm Be + CH PCD response curve.....
     +   npTX  = 1500, ! Number of v-points on MCP product curve
     +   K13po = 119,         ! Number of points in linear interpolation of spherical K13(k*R) function 
     +   nHDpo=35, nTARpo=40  ! for single-cell K13 approach, number of H/D points & tauR points

      real(8), public, parameter ::    
     +    c = 2.9979246d10,       ! cm/s  ADT
c         e =	4.8032d-10          ! CGS, not used 
     +    BolJ = 1.6021773d-19,   ! J/eV  Boltzmann
     +    BolEr= 1.6021773d-12,   ! Erg/eV  Boltzmann
     +    elma = 9.1093897d-28,   ! g; electron mass
c    +    Uam  = 1.6605402d-24,   ! g; atomic mass unit
     +    h = 4.1356692d-15,      ! eV/Hz Planck
     +    eVA = 12398.424d0,      ! A*eV;   hv(eV)*Lambd(A)
     +    pin = 3.14159265d0,
     +    ToPi = 2.d0*pin, 
     +    FoPi = 4.d0*pin, 
     +    sqpi = 1.77245385d0,    ! sqrt(pin)
     +    Ampli= 2.362d-13,       ! 8*pi/sqrt(3) * pi*ao^2 * Ry^2  for VR       
     +    zero = 0.d0,
     +    one = 1.d0,
     +    two = 2.d0,
     +    three = 3.d0,
     +    third = one/three,
     +    tol = 1.d-7,        ! tolerance in NAG d01;  must be <= 10^-7. Checked by using equal GT for cells #1 & #3 in z1518 with RF=0
     +    tolD02 = 1.d-6      ! tolerance in NAG d02 

      character*5, public :: QSname1(NSTm,nXE)    ! part 1, according to 'IonzXX.inp' list
      character*5, public :: QSname2(NSTm,nXE)    !      2
      character*9, public :: title, empty, comme  ! in input files reading

      integer, public :: 
     +  nuQS(HSSm,nXE),   ! Amount of ELs in each SpS
     +  nuAS(HSSm,nXE),   ! Amount of AutoIonizing (AI) ELs in each SpS
     +  nuGS(HSSm+1,nXE), ! numbers corresponding to grpund levels of each SpS, including nucl (for each XE).
     +  kAI1(HSSm,nXE), kAI2(HSSm,nXE),  ! for AI ELs of each SS, their first & last # in total list of ELs
     +  kiSS(NSTm,nXE), nprin(NSTm,nXE), ! SS of each EL & XE;  For each EL: principal quantum number of outer shell
     +  MthdEX(NSTm,NSTm,nXE),           ! formula # for fitting e-impact excitation cross-section 
     +  nX3(MNLe), nUp(MNLe), nLo(MNLe), ! XE and EL-List numbers of Upper & Lower levels of spectral line after hv-regulating
     +  nUph(MNLe,nXE), nLoh(MNLe,nXE),  ! same prior to hv-regulation
     +  LastAI(nXE),   ! EL List# of the last AI EL;  calculated in "Intro"
     +  linM(nXE),     ! number of term-to-term lines in [hvmin, hvmax] domain, except those to be FLAG-replaced by their FS components
     +  PoLeNu(nXE,6), ! For each XE, Six levels chosen for printing their POP(t) in file "POPs_BS_AL" & "POPs_BS_MG". 
     +  gpEq(mSpe),    ! serial numbers of t point at which t = tres(...)
     +  KeRedu,    ! -1, 0 or 1 from FLAG. Ionization potential lowering model: none (-1), IonSphe (0) or DebHu (1). 
     +  La, Law,   ! serial # of plasma zone under study (w-layer). 
     +  KeySmooth, ! > 1 means "Replace broken-line t-depe of params with smooth Lor-type t-depe"
     +  tresPeak,  ! # = 1,2, ...,6 in "tres(#)" chosen for the peak of Te(t)
     +  linM4,     ! number of spectral lines in [hvmin, hvmax] domain (sum over XE)  
     +  nX, n7, ii, ! Chemical Element (XE) number & w-variable for XE#
     +  iTX1, iTX2,        ! 1st & last v-point within TREX v-range  
     +  i, iv, kw, k1, k2, ! w-variables 
     +  iSS, jSS, kQS, kS, ! w-variables 
     +  k, k3, kf, j, ng, 
     +  kU, kL, lin, nLam, ! w-variables
     +  npts, Nlim, Ifail, ! used in D01ahf and d02
     +  iv1770,            ! for ScaF 
     +  PrFr,   ! = 1,2,3,4 is "tres(#)" for printing "Resume" & "UnderstandPCD"  
     +  npa     ! number of parts of each (ti-tf] interval, i.e. including "tf". Now 4 <= npa <= 48

      real(8), public :: 
     +   EmTot(LaMx,nvM),           ! plasma emissivity [W/cc/sr/eV] in LaMx zones. 
     +   AbTot(LaMx,nvM),           ! plasma absorption coefficient corrected for stimulated emissiony [1/cm]
     +   absorBF(nvM),  emisFB(nvM),   ! [1/cm], [W/cc/sr/eV] 
     +   SpInEff(LaMx,nvM),            ! [W/cm2/sr/eV] Space- direction- average spectral intensity of radiation in BS 
     +   Yield(nvM), Yietf,  ! Radiation spectral yield [J/eV], Radiation yield [J/sr] up to present 'tf', t-inter of Power
     +    E(NSTm,nXE),       ! EL Energy relative to THIS-SS ground state
     +   g0(NSTm,nXE),   ! EL degeneracy: table value
     +   PI (HSSm,nXE),  ! Ionization Potential of each GS, table value
     +   PIR(HSSm,nXE),  ! Ionization Potential of each GS, reduced by continuum lowering
     +   DPI(HSSm,nXE),  ! Reduction of Ionization Potential according to 'KeRedu'
     +   BE(NSTm),           ! Binding Energy of each EL 'ti'-corrected in 'AtKins' after continuum lowering
     +   BEp(NSTm,nXE,LaMx), ! after 'call AtKins', 'tf'-Binding Energy of w-layer is saved until next visit to this 'La'; then previous-t BE may be needed for restart of dead EL.
     +   bra(NSTm,NSTm,nXE), ! with FAC bases, we assign bra= 1/0 based on yes/no single-e ioniz cross-Sec for this i/f couple in "InzXE....inp" file.  
     +     A(NSTm,NSTm,nXE), ! Einstein A coef [1/s]
     +  WAiz(NSTm,NSTm,nXE), ! AutoIoniz Probability [1/s]
     +   flu(NSTm,NSTm,nXE), ! Correct-sign (>0) Absorption Oscillator Strength
     +   fVR(NSTm,NSTm,nXE), ! Pseudo "Absorption Oscillator Strength" for Van-Regemorter formula, if needed 
     +    Ax(NSTm,NSTm,nXE), ! Ax-Fx are 6 coefs used in fitting formulae for e-impact excitation cross-section, see Excit.inp
     +    Bx(NSTm,NSTm,nXE),  
     +    Cx(NSTm,NSTm,nXE), 
     +    Dx(NSTm,NSTm,nXE),
     +    Ex(NSTm,NSTm,nXE), 
     +   Aix(NSTm,NSTm,nXE), ! Aix-Dix are 4 coefs of e-impact ioniz cross-section, see "InzXE....inp"
     +   Bix(NSTm,NSTm,nXE),  
     +   Cix(NSTm,NSTm,nXE), 
     +   Dix(NSTm,NSTm,nXE),
     +   Eix(NSTm,NSTm,nXE), ! Eix-Hix are 4 coefs of photo-ioniz cross-section, see "InzXE....inp"
     +   Fix(NSTm,NSTm,nXE), 
     +   Gix(NSTm,NSTm,nXE), 
     +   Hix(NSTm,NSTm,nXE), 
     +   Eth(NSTm,NSTm,nXE), ! State-to-state Ionization Threshold (for ioniz cross-secs) 

     +   WI(NSTm,NSTm),      ! single ionization probability (1/s), removal of one electron per e-impact
     +   WMI(NSTm,NSTm),                   ! multiple ionization probability (1/s), removal of 2,3... electrons per e-impact
     +   WEX(NSTm,NSTm), WDX(NSTm,NSTm),   ! e-impact excitation & deexcitation probabilities (1/s)
     +   WRR(NSTm,NSTm), WTB(NSTm,NSTm),   ! radiative & 3-body recombination probabilities (1/s)
     +   WInd(NSTm,NSTm), Wab(NSTm,NSTm),  ! induced emission & Photo(BB)absorption probabs (1/s)
     +   WPhI(NSTm,NSTm), WiRR(NSTm,NSTm), ! photoIonization & induced RR probabs, 1/s
     +   WDC(NSTm,NSTm),                   ! probability of Dielectronic Capture into AI EL of He-like & Li-like ions
     +   PM(NSTm,NSTm),           ! Probability Matrix [1/s]
     +   Wout(NSTm, nXE, LaMx),   ! EL depletion rate, PM(k,k)=-Wout(k) 
     +   POPt(NSTm,nXE,LaMx),     ! relative abundance of ELs 
     +   POPZ(HSSm+1,nXE,LaMx),         ! relative abundance of ionization stages, including nucl, which has SS= HSS+1 
     +   hvTX(npTX),   RespTX(5,npTX),  ! TREX response (product) in 5 orders of mice reflection 
     +   ReTX(5,nvM), SpOutOr(5,nvM),   ! 5 orders of mica 
     +   Tab1(5,nvM), SumTab1(nvM),         ! Gamma(jhv)*Teta(khv,Te); Gamma==ReTX(j,iv); Teta==Emi/ni^2;  For Table 1 of the paper:j-Sum of "Tab1"
     +   hv8Be(npBe8),   RespoBe(npBe8),    ! hv points of response of PCD-Be (that with Beryllium filter), 8mcm Be+CH transmi * abso of 0.5mm diamond 
     +   hv5kap(np5kap), Resp5kap(np5kap),  ! hv points of response of PCD-5  (that with  5 mils kapton filter)  [1mil = 25.4 mcm; mils= mInch] 
     +   hv10ka(np10ka), Resp10ka(np10ka),  ! hv points of response of PCD-10 (that with 10 mils kapton filter)
     +   hv40ka(np40ka), Resp40ka(np40ka),  ! hv points of response of PCD-40 (that with 40 mils kapton filter). PCD response ~ filter transmi * diamond abso
     +   kRp(k13po), Ksp(k13po),            ! 119-point fit to spherical K13 function Ks(kR) 
     +   TePr(4), nALpr(4), nePr(4), 
     +   PopHe(4), PopH(4), PopLiGS(4), 
     +   SpOut1(nvM), Conv2(nvM),        ! z-average SpIn along LOSs #1 (centr) & LOS #2 (off-axis) of dz1
     +   RadFlux(nvM), Ar(LaMx+2), 
     +   hvCh(MNLe,nXE), hvC(MNLe), ! Spectral line Centers before & after hv-regulation
     +   hvV(nvM),                  ! sequence of hv points on hv axis [eV] 
     +   tres(mSpe),                ! instants for changing params & prints
     +   Dfr(mSpe), TeFr(mSpe), TDfr(mSpe), nFr(mSpe), peMG(mSpe),  ! tres-values [cm, eV, keV, i/cc, perc]
     +   FWkVcGAU(nXE), ! FWHMgau(TD)/hvC; equal for all lines of XE,La. 
     +   FWevLor(MNLe), ! Lorentzian (life-time or FLAG-Stark) part of FWHM [eV] for Voigt shape 
     +   FWvoi(MNLe),              ! FWHM of Voigt line shape, eV    
     +   HtoD(nHDpo), RpK(nTARpo), ! For single-cell K13 RF: values of 'H/D' & 'k*R'	      
     +   fK13(nHDpo,nTARpo),       ! input K(H/D,k*R) martix; see read (102)
     +   AtMass(nXE), Imas(nXE),  ! ion mass in [a.u.] & [g].   Imas(nX)= AtMass(nX)*1.66054d-24 
     +   Volu(LaMx),        ! cm^3; Volume of cylindric shell
     +   ZC(nXE),      ! each-XE mean ion charge (for screen & MHD printouts; for HoltSum, ...) compu in the end of AtKins together with POPZ
     +   Zbar(LaMx),   ! mean ion charge in plasma of Z1520 AL+Mg alloy 
     +   WEAF(Nwork),  ! NAG WorkArea 
     +   Den(nXE),     ! ion number density for all XE & w-XE;  i/cc 
     +   tgp(300),     ! t-points of t axis, Can be done bigger but usually we use ~100 of 300
     +   DE, BEk,      ! E betw two levels under consideration, E required for any 'k'-->'kf' transition, init energy of free el in fb trans. 
     +   Scale,        ! t-Scale=10^40 for d02
     +   SumP, tstep,      ! sum POP 
     +   ti, tf, strt, tm, ! time-step [s]. Initial & final instants of present time-step: tf= ti+ tstep; tm=(ti+tf)/2. Start-value of "ti".
     +   tiS, tfS,         ! NAG params and WorkArea 
     +   R2,               ! radius of BS [cm]
     +   Tion, Te,   ! ion & electron temperature in w-layer [eV], prescribed (Scenario) 
     +   TiD,        ! Doppler temperature of thermal+3Dhydro motion of ions    
     +   paMG,       ! ions of MG part in total ion number density DenI
     +   DenI, Dene, ! total ion & electron density, i/cc & e/cc
     +   ZC1, ZC2,   ! mean (all-XE, all-SpS, inclu atoms) ion charge and charge^2 for 'redPI' subr 
     +   Sah,        ! Sah = Dene*(one-bp)/(6.03717d21*Te**1.5d0)
     +   InvDebRad,  ! inverse debye radius [1/cm]
     +   bp, bc, bw, ! params of e-beam for non=Maxw EED (given by "bp > 0."), see EED function 
     +   AulMin,     ! Min allowed value of Einst A() coef. If A() < AulMin, we replace it by A() =0 in "Intro" 
     +   fluMin,        ! Min allowed value of Abso OscStrenght. If f() < fluMin, we replace A bt 0 in "Intro" 
     +   hvMin, hvMax,  ! edges of full hv interval [eV]; given in FLAG.inp
     +   hvSmo,         ! edge of [hvMin,hvSmo] domain, where lines have large FWHM (because of W >> A) & do not need fine resolution; comes from FLAG
     +   hvMiF, hvMaF,  ! edges of Super-Fine hv interval 
     +   hveV, RadPow,  ! outgoing Radiation Power thru plasma surf [W]
     +   rer,           ! used in D01ahf(...)
     +   freqEI,      ! in Braginskii
     +   CouL, RO,    ! Coulomb Log, mass density RO 
     +   wtot, wtot1,           ! w-var for CPpCm in 'AtKins' 
     +   POPk, POPkr, popgrat,  ! used for PhI with Geff 
     +   whv, wtra, ww, Du,     ! w-variables
     +   distII, TeBS,          ! used in functions SigExc, SigInz, SigPhi for avoiding too large values via FAC fit errors.  No corre in REVERSE processes as (1) they are expre via direct & (2) must provide equilibration.             
     +   dDu, teiBr, teiE,      ! e-i time of momentum transfer & energy transfer 
     +   TeCO, neCO, teiTco, TeDot, 
     +   TePrCO, tmPrCO, SoCO, abCO, TeDotPow, 
     +   TiBS, MeanM, dhv, ReverseKKF, Wing,   
     +   xx1, xx2, xx3, xx4, Frac, TX1, TX2, RadY, 
     +   Lac, drr, rMid, GTe, niBar0, Mx1, Vx, 
     +   Dstrt, TeStrt, TDstrt, nStrt, peMGst,  ! initial values [cm, eV, eV, i/cc   % ]          
     +   dtW, tip, HWL1, HWL2, PeakTe,          ! HWHM of Lor Te(t) before & after Te^; peak value of Te(t) 
     +   EzDot, EzPr          
       

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
      end module	  


      module mo1               !  Feb 14, 2024
      implicit none
      integer, public, parameter :: 
     +     nXE= 2              ! number of chemical elements (XE) in present simulation: nX=1 is Kr, nX=2 is MG. 
      integer, public :: 
     +     FSS(nXE), HSS(nXE), ! First & H-like Spectroscopic Symbol (SS) in database; FSS may be any, but it must have > 1 QSs (at least RY).
     +     Nnu(nXE),           ! Number of "regular" Quantum States (QSs) in databases == nucl#; this number does not include AI & Rydberg QSs
c                                In "Intro" & ionization we assume that state #Nnu(nX) is nucleus  
     +     NST(nXE)  ! TOTAL number of QSs on the list: "regular" +AI 

      DATA FSS / 30,  9/,   ! all Be-like SSs of Kr & MG
     +     HSS / 36, 12/,   !     H -like SSs
     +     Nnu /298, 109/,  ! nuclei # in the QS list of XE. It == the number of "regular" QSs (not AI) 
     +     NST /1612, 272/   ! number of states in the databases of Kr and Mg  
      integer, public, parameter :: 
     +     NSTm = 1612,             ! max of two "NST"	   
     +     HSSm =  36,                  ! highest (of all XE) SS of H-like ion
     +    Nwork = 50+ 12*NSTm+ NSTm**2  ! For NAG d02eaf (Gear) workArea 'WEAF'	

      integer, public, parameter :: 
     +   nFAI = 301 ,   ! in each SS, containing AI QSs, serial number of lowest AI QS.                             
     +   nvM  = 12300, ! number of hv-grid points; 100 points in soft [hvMin, hvSmo] domain & 200 points in [hvMaxf, hvMax] domain 
     +   mSpe = 6,     ! Number of spectra prints: files 61 - 76,171-176, 371-376, 
     +   MNLe = 11566,  ! Not more than "MNLe" spectral lines are expected for all XE together (common LineList)
     +   LaMx = 2,     ! Number of cylindrical plasma shells in present run: HOT Interm(La=1), Vacuum(La=2) CORE (La=3) & COLD HALO(La=4)
     +   np5kap= 2272, ! Number of v-points on  5 mils kapton PCD response curve [1mil = 25.4 mcm; mils= mInch] 
     +   np10ka= 2301, ! Number of v-points on 10 mils kapton PCD  
     +   np40ka= 2194, ! Number of v-points on 40 mils kapton PCD  
     +   npBe8 = 2455, ! Number of v-points on 8 mcm Be + CH PCD response curve.....
     +   npTX  = 1500, ! Number of v-points on MCP product curve
     +   K13po = 119,         ! Number of points in linear interpolation of spherical K13(k*R) function 
     +   nHDpo=35, nTARpo=40, ! for single-cell K13 approach, number of H/D points & tauR points
     +   JpoMx = 1000	        ! Max number of points in Jenya's computation of MgLyB shape

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
     +  nuQS(HSSm,nXE),   ! Amount of QSs in each SS
     +  nuAS(HSSm,nXE),   ! Amount of AutoIonizing (AI) QSs in each SS
     +  nuGS(HSSm+1,nXE), ! QS## corresponding to GSs of each SS, including nucl.
     +  kAI1(HSSm,nXE), kAI2(HSSm,nXE),  ! for AI QSs of each SS, their first & last # in total list of QSs
     +  kiSS(NSTm,nXE), nprin(NSTm,nXE), ! SS of each QS & XE;  For each QS: principal quantum number of outer shell
     +  MthdEX(NSTm,NSTm,nXE),           ! formula # for fitting e-impact excitation cross-section 
     +  nX3(MNLe), nUp(MNLe), nLo(MNLe), ! XE and QS-List numbers of Upper & Lower levels of spectral line after hv-regulating
     +  nUph(MNLe,nXE), nLoh(MNLe,nXE),  ! same prior to hv-regulation
     +  LastAI(nXE),   ! QS List# of the last AI QS;  calculated in "Intro"
     +  linM(nXE),     ! number of term-to-term lines in [hvmin, hvmax] domain, except those to be FLAG-replaced by their FS components
     +  PoLeNu(nXE,6), ! For each XE, Six levels chosen for printing their POP(t) in file "POPs_BS_AL" & "POPs_BS_MG". 
     +  gpEq(mSpe),    ! # of t-grid point at which t = tres(...)
     +  KeRedu,    ! -1, 0 or 1 from FLAG. Ionization potential lowering model: none (-1), IonSphe (0) or DebHu (1). 
     +  La, Law,   ! serial # of plasma layer under study (w-layer). 
     +  KeySigMax, ! = 2 or 0 means Yes/No restriction of of FACn cross-secs (Exc, Inz, Phi) by "SigMax" 
     +  KeySmooth, ! > 1 means "Replace broken-line t-depe of params with smooth Lor-type t-depe"
     +  tresPeak,  ! # = 1,2, ...,6 in "tres(#)" chosen for the peak of Te(t)
     +  linM4,     ! number of spectral lines in [hvmin, hvmax] domain (sum over XE)  
     +  nX, n7, ii, ! Chemical Element (XE) number & w-variable for XE#
     +  iTX1, iTX2,         ! 1st & last v-point within TREX v-range  
     +  i, iv, kw, k1, k2,  ! w-variables 
     +  iSS, jSS, kQS, kS,  ! w-variables 
     +  k, k3, kf, j, ng, 
     +  kU, kL, lin, nLam,  ! w-variables
     +  npts, Nlim, Ifail,  ! used in D01ahf and d02
     +  iv1450, iv1472p3, iv1485,   ! around MgLyA 
     +  iv1695, iv1704p2, 
     +  iv1740, iv1744p6, iv1757,  ! around MgLyB   
     +  iv1758, iv1765, iv1770, 
     +  nC,                 ! # of central point of r-grid for vertical (radial) scans 
     +  ivHi1, inU,         ! index used in shifting local (u=0) EmTot(La,iv,1) & AbTot(La,iv,1) according to uLOS(La) along a few LOS (inU= 2,3,...)
     +  PrFr,       !  = 1,2,3,4 is "tres(#)" for printing "Resume" & "UnderstandPCD"  
     +  YesDip, ! YesDip = 0 means "NO DIP", YesDip = 1 means "YES DIP"
     +  npa     !  number of parts of each (ti-tf]

      logical, public :: isDead4kg3(NSTm)  ! flags, used in Geff calculations. isDead4kg3 is only used when keyGeff = 3

      integer, public :: indexF(NSTm), indexB(NSTm) 

      real(8), public :: 
     +   EmTot(LaMx,nvM,10), ! plasma emissivity [W/cc/sr/eV] in LaMx shells. 3rd index ("inU") is used for v-shifting "EmTot" according to towards-detector velocity.
c                             inU=1 for lab frame (U=0); inU= 2,4,6,8 for Far-from-detector parts of 4 LOSs; inU= 3,5,7,9 for Near-detector parts of 4 LOSs.
c                             we cannot use only inU=1,2,3 because EmTot & AbTot are transferred from "RTeSpIn subr" to "EffSpIn subr".
c                             Diagnostic LOS#4 is used in frame-times only (and AFTER "EffSpIn") BUT we keep inU= 8,9 for U-shifts along this LOS to avoid confusion. 
c                             In same "TREXscans" subr, inU=8,9 are later used for shifts along chordal scans  
     +   AbTot(LaMx,nvM,1),            ! plasma absorptivity [1/cm] in the same shells for the same "inU" values.
     +   absorBF(nvM),  emisFB(nvM),   ! [1/cm], [W/cc/sr/eV] 
     +   abBFcon(nvM), emFBcon(nvM),   ! [1/cm], [W/cc/sr/eV] 
     +   SpInEff(LaMx,nvM),            ! OMEGA-mean RF intensity in 3 shells [W/cm2/sr/eV]
     +   SpToHA(nvM),        ! from BS to HALO along los at r=Rco/2
     +   Yield(nvM), Yietf,  ! Radiation spectral yield [J/eV], Radiation yield [J/sr] up to present 'tf', t-inter of Power
     +    E(NSTm,nXE),       ! QS Energy relative to THIS-SS ground state
     +   g0(NSTm,nXE),   ! QS degeneracy: table value
     +   PI (HSSm,nXE),  ! Ionization Potential of each GS, table value
     +   PIR(HSSm,nXE),  ! Ionization Potential of each GS, reduced by continuum lowering
     +   DPI(HSSm,nXE),  ! Reduction of Ionization Potential according to 'KeRedu'
     +   Price(NSTm,nXE),    ! QS energy relative to E(1). Commonly GS ATOM has E=0, but it may have E>0 if term; "Price" accounts for continuum lowering
     +   BE(NSTm),           ! Binding Energy of each QS 'ti'-corrected in 'AtKins' after continuum lowering
     +   BEp(NSTm,nXE,LaMx), ! after 'call AtKins', 'tf'-Binding Energy of w-layer is saved until next visit to this 'La'; then previous-t BE may be needed for restart of dead QS.
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
     +   WDC(NSTm,NSTm),                   ! probability of Dielectronic Capture into AI QS of He-like & Li-like ions
     +   PM(NSTm,NSTm),        ! Probability Mtrx [1/s]
     +   Wout(NSTm, nXE, LaMx),   ! QS depletion rate, PM(k,k)=-Wout(k) 
     +   POPt(NSTm,nXE,LaMx),     ! relative abundance of QSs 
     +   POPZ(HSSm+1,nXE,LaMx),         ! relative abundance of ionization stages, including nucl, which has SS= HSS+1 
     +   hvTX(npTX),   RespTX(5,npTX),  ! TREX response (product) in 5 orders of mice reflection 
     +   ReTX(5,nvM), SpOutOr(5,nvM),   ! 5 orders of mica 
     +   Tab1(5,nvM), SumTab1(nvM),         ! Gamma(jhv)*Teta(khv,Te); Gamma==ReTX(j,iv); Teta==Emi/ni^2;  For Table 1 of the paper:j-Sum of "Tab1"
     +   hv8Be(npBe8),   RespoBe(npBe8),    ! v-grid & PCD response for 1520 K-shell Power Beryllium filter (8mcm Be+CH transmi * abso of 0.5mm diamond) 
     +   hv5kap(np5kap), Resp5kap(np5kap),  ! v-grid & PCD response for  5 mils kapton filter [1mil = 25.4 mcm; mils= mInch] 
     +   hv10ka(np10ka), Resp10ka(np10ka),  ! v-grid & PCD response for 10 mils kapton filter
     +   hv40ka(np40ka), Resp40ka(np40ka),  ! v-grid & PCD response for 40 mils kapton filter transmi * diamond abso
     +   kRp(k13po), Ksp(k13po),            ! 119-point fit to spherical K13 function Ks(kR) 
     +   TePr(4), nALpr(4), nePr(4), u3Dpr(4),
     +   PopHe(4), PopH(4), PopLiGS(4), 
     +   SpOut1(nvM), Conv2(nvM),        ! z-average SpIn along LOSs #1 (centr) & LOS #2 (off-axis) of dz1
     +   RadFlux(nvM), Ar(LaMx+2), 
     +   hvCh(MNLe,nXE), hvC(MNLe), ! Spectral line Centers before & after hv-regulation
     +   hvV(nvM),                  ! hv-grid [eV] 
     +   tres(mSpe),                ! instants for changing params & prints
     +   Dfr(mSpe), TeFr(mSpe), TDfr(mSpe), nFr(mSpe), peMG(mSpe),  ! tres-values [cm, eV, keV, i/cc, perc]
     +   Ysize(LaMx),   ! For plane geometry or shell thickness in cyl geom
     +   uLOS(LaMx),    ! layer (shell) velocity along LOS; given in Scenario OR ZeroD. Used for spectral line shifts.
     +   FWkVcGAU(nXE), ! FWHMgau(TD)/hvC; equal for all lines of XE,La. 
     +   FWevLor(MNLe), ! Lorentzian (life-time or FLAG-Stark) part of FWHM [eV] for Voigt shape 
     +   FWvoi(MNLe),              ! FWHM of Voigt line shape, eV    
     +   HtoD(nHDpo), RpK(nTARpo), ! For single-cell K13 RF: values of 'H/D' & 'k*R'	      
     +   fK13(nHDpo,nTARpo),       ! input K(H/D,k*R) martix; see read (102)
     +   AtMass(nXE), Imas(nXE),  ! ion mass in [a.u.] & [g].   Imas(nX)= AtMass(nX)*1.66054d-24 
     +   EinzEx(nXE,LaMx),        ! eV, Ioniz+Excit energy per ion
     +   Volu(LaMx),        ! cm^3; Volume of cylindric shell
     +   ZC(nXE),      ! each-XE mean ion charge (for screen & MHD printouts; for HoltSum, ...) compu in the end of AtKins together with POPZ
     +   Zbar(LaMx),   ! mean ion charge in plasma of Z1520 AL+Mg alloy 
     +   WEAF(Nwork),  ! NAG WorkArea 
     +   Den(nXE),        ! ion number density for all XE & w-XE;  i/cc 
     +   MaBSvsMw(LaMx),  ! Mass of 1 BS vs Mw
     +   tgp(300),        ! t-points of t-grid, 300 is overestimate
     +   DE, BEk, Euu, Ell,      ! E betw two levels under consideration, E required for any 'k'-->'kf' transition, init energy of free el in fb trans.
     +   Scale,        ! t-Scale=10^40 for d02
     +   SumP, tstep,      ! sum POP 
     +   ti, tf, strt, tm, ! time-step [s]. Initial & final instants of present time-step: tf= ti+ tstep; tm=(ti+tf)/2. Start-value of "ti".
     +   tiS, tfS,         ! NAG params and WorkArea 
     +   ct1, ct2, ww,       ! edges of time interval chosen for printout of long detailed comments
     +   R1, R2, R3, R4, R5, ! more practical than "CyR"
     +   SiW, VacR,          ! w-variable; radius of vacuum shell inside 1st cyl shell. 
     +   Du, u3D,    ! dif betw uLOS(La);  velocity of 3D MHD motion in the shell (given in Scenario OR ZeroD). Used for broadening of spectral lines.
     +   Tion, Te,   ! ion & electron temperature in w-layer [eV], prescribed (Scenario) or 0D-MHD (ZeroD) 
     +   TiD,        ! Doppler temperature of thermal+3Dhydro motion of ions    
     +   paMG,       ! ions of MG part in total ion number density DenI
     +   DenI, Dene, ! total ion & electron density, i/cc & e/cc
     +   ZC1, ZC2,   ! mean (all-XE, all-SS, inclu atoms) ion charge & char^2;  needed for 'redPI' subr & ZeroD (scattering of free electrons)
     +   Sah,        ! Sah = Dene*(one-bp)/(6.03717d21*Te**1.5d0)
     +   InvDebRad,  ! inverse debye radius [1/cm]
     +   bp, bc, bw,     ! params of e-beam for non=Maxw EED (given by "bp > 0."), see EED function 
     +   AulMin,         ! Min allowed value of Einst A() coef. If A() < AulMin, we replace it by A() =0 in "Intro" 
     +   fluMin,         ! Min allowed value of Abso OscStrenght. If f() < fluMin, we replace A bt 0 in "Intro" 
     +   ExInzE, ExInzR, ! Ioniz+Excit part of Internal E [eV/HP] & it's Growth rate [eV/s/HP]
     +   hvMin, hvMax,   ! edges of the v-grid [eV] from FLAG
     +   hvSmo,          ! edge of [hvMin,hvSmo] domain, where lines have large FWHM (because of W >> A) & do not need fine resolution; comes from FLAG
     +   hvMiF, hvMaF,   ! edges of Super-Fine hv-grid; 
     +   BroIns,         ! instrumental broadening of spectral lines; == vC/FWHMinstr for thin line
     +   hveV, RadPow,   ! outgoing Radiation Power thru plasma surf [W]
     +   rer,               ! used in D01ahf(...)
     +   OmegCicl, freqEI,  ! in ZeroD & Braginskii
     +   CouL, RO,          ! Coulomb Log, mass density RO 
     +   Eint, Ehyd,        ! Internal & hydro energy [J] in cylindrical (non-expanding, 1D) plasma jet of laser-spot diameter
     +   wtot, wtot1,       ! w-var for CPpCm in 'AtKins' 
     +   fwTiKvC, fwTUKvC,      ! for-broadening info
     +   POPk, POPkr, popgrat,  ! used for PhI with Geff 
     +   e2U2,                  ! param <e^2*U(r=0)^2> = k*Te*(e^2/DebR)/(1+ 1/Zbar) for Gaus distr in "GeffSigPhi" func
     +   marge2u2, whv, wtra,        ! sqrt(60*e2u2); w-variables
     +   SigMax, rPSI, distII, rCRO, ! used in functions SigExc, SigInz, SigPhi for avoiding too large values via FAC fit errors.  No corre in REVERSE processes as (1) they are expre via direct & (2) must provide equilibration.             
     +   dL2, dL2c, dL3, dL33, dL4,  
     +   TeBS, u3Dcore,  EmaxVR,     ! max DE for replasement of FACn SigExc by VR formula (using FACn "f") for DE= [0,EmaxVR]    
     +   TruH, TruC, TruI,           ! Through-Plasma path of LOS in hole, core, interm, while (1-Tru) part of the path is thru empty part of the shell
     +   UrInCo, UrHaIn,    ! On-interface Implosion velo required for mass conserv 
     +   Ener,              ! J,    Sum of above two, sum over the shells
     +   dDu, teiBr, teiE,          ! tEI for momentum transfer & energy transfer 
     +   TeCO, neCO, teiTco, TeDot, 
     +   TePrCO, tmPrCO, SoCO, abCO, TeDotPow, 
     +   RadPerIon, TiCor, MeanM, dhv, ReverseKKF,
     +   xx1, xx2, xx3, xx4, Frac, TX1, TX2, RadY, MxToMw,
     +   Lac, drr, rMid, GTe, niBar0, Mx1, Vx, PrcPCDBe, 
     +   expPCD40, expPCD10, expPCD53, expPCD54, expPCDBe, expXRD, 
     +   PCD40, Wing, FWHMA, FWHMB, hvBlue, hvRed, P10toP5,
     +   MgLyAmin, MgLyAmax, MgLyBmin, MgLyBmax, expPCD5,
     +   ALLyA_FW_Gau, ALLyA_FW_Natu, ALLyA_FW_StJe,
     +   ALLyA_FW_Lor, ALLyA_FW_Voi,
     +   MgLyA_FW_Gau, MgLyA_FW_Natu, MgLyA_FW_StJe,
     +   MgLyA_FW_Lor, MgLyA_FW_Voi, 
     +   FWHM_JV,  FWHM_JVTD,  FWHM_JVTD10, FWHM_JVTD10i,       
     +   Top, hvTop, Bottom, hvDip,
     +   Dstrt, TeStrt, TDstrt, nStrt, peMGst,  ! initial values [cm, eV, eV, i/cc   % ]          
     +   dtW, tip, HWL1, HWL2, PeakTe,          ! HWHM of Lor Te(t) before & after Te^; peak value of Te(t) 
     +   EzTot1i, EzTot, EzDot, EzPr, TeDotInzEx          
       

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


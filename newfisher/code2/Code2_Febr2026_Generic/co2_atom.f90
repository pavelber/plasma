!  Code2 (Co2feb26), Generic version. Module split (Nov 2026):
!    co2_const  - constants and fixed dimensions
!    co2_atom   - atomic database of the elements, rate matrices and populations,
!                 channel lists; their allocation (AllocArrays, AllocSized, AllocLines)
!    co2_util   - wall-clock timers, integration context of D01AHF, FlushAll
!    mo1co2feb  - state of the plasma, grids, spectra, run control; it uses the three
!                 modules above, so 'use mo1co2feb' gives access to everything.
      module co2_atom
      use co2_const
      implicit none
      integer, public :: &
     &     FSS(nXE), HSS(nXE), &  ! First & H-like Spectroscopic Symbol (SpS) in database (DaBa); FSS can be any, but it must have > 1 energy levels (ELs)
     &     Nnu(nXE), &  ! nuclei order # in databases = the number of "non-AI" Energy Levels (ELs)
     &     NST(nXE)  ! number of ELs in each of 4 DaBa [non-AI and AI] : .

!  Array sizes that depend on the chemical element are NOT compile-time
!  parameters in this Generic version: AllocArrays() (CONTAINS part below)
!  sets nQSm= max(NST), HSSm= max(HSS) from Params0.inp at run time,
!  AllocLines() sets MNLe from the number of spectral lines (LineList).
      integer, public :: &
     &     nQSm = 0, &  ! number of ELs: max(NST(1:nXE)) of Params0.inp
     &     HSSm = 0, &  ! max SpS of H-like ion: max(HSS(1:nXE)) of Params0.inp
     &    Nwork = 0, &  ! 50+ 12*nQSm+ nQSm**2, for NAG d02eaf work Area WEAF
     &     MNLe = 0  ! number of spectral lines + 1, set in LineList
      character*24, public, allocatable :: QSname(:,:)  ! part 1, according to 'Inz.inp' list
      integer, public, allocatable :: &
     &  nuAS(:,:), &  ! Number of AutoIonizing (AI) ELs in each SpS
     &  nuQS(:,:), &  ! Number of non-AI ELs in each SpS via "QSs.inp"
     &  nuGS(:,:), &  ! EL# of ground state ("GrSt")'s of each SpS, including nucl.
     &  kAI1(:,:), kAI2(:,:), &  ! for AI ELs of each SpS, their first & last # in total list of ELs
     &  kiSS(:,:),  pqn(:,:), &  ! SpS of each EL & XE;  For each EL: principal quantum number of outer shell
     &  nu1(:), nu2(:), &  ! Level order # in Z-stage, Level order ## in total list
     &  eqEL(:,:),  orb(:,:), &  ! number of equivalent electrons in nl-subshell; orbital q.n.
     &  MthdEX(:,:)  ! formula # for fitting e-impact excitation cross-section of high-Z dopant
      integer, public, allocatable :: &
     &  nX3(:), nUp(:), nLo(:), &  ! XE and EL serial numbers of Upper & Lower levels of spectral line after ordering the lines over hv
     &  nUph(:,:), nLoh(:,:)  ! same prior to the ordering
      real(8), public, allocatable :: &
     &     LvJW(:,:,:), &  ! LEVEL Stark width [eV], J-formulary
     &   LvLorW(:,:,:), &  ! same corrected for uncertainty principle
     &    PI(:,:), &  ! Ionization Potential of each GS, table value
     &   PIR(:,:,:), &  ! Ionization Potential of each GS, reduced by continuum lowering
     &   DPI(:,:,:), &  ! Continuum lowering

     &     E(:,:), &  ! EL Energy relative to THIS-SS ground state
     &    g0(:,:), &  ! EL degeneracy: table value
     &    BE(:,:,:), &  ! Binding Energy of each EL 'ti'-corrected in 'AtKins' after continuum lowering
     &   BEp(:,:,:), &  ! after 'call AtKins', 'tf'-Binding Energy of w-layer is saved until next visit to this 'La'; then previous-t BE may be needed for restart of dead EL.
     &   bra(:,:,:), &  ! with FAC bases, we assign bra= 1/0 based on yes/no single-e ioniz cross-Sec for this i/f couple in "InzXE....inp" file.
     &     A(:,:,:), &  ! Einstein A coef [1/s]
     &  WAiz(:,:,:), &  ! AutoIoniz Probability [1/s]
     &   flu(:,:,:), &  ! Correct-sign (>0) Absorption Oscillator Strength
     &    Ax(:,:), &  ! Ax-Fx are 6 coefs used in fitting formulae for
     &    Bx(:,:), &  ! e-impact excitation cross-sections of
     &    Cx(:,:), &  ! high-Z dopant, see Exc.inp
     &    Dx(:,:), &
     &    Ex(:,:), &
     &    Fx(:,:), &
     &   Aix(:,:), &  ! Aix-Dix are 4 coefs of e-impact ioniz cross-section of high-Z dopant, see "Inz.inp"
     &   Bix(:,:), &
     &   Cix(:,:), &
     &   Dix(:,:), &
     &   Eix(:,:), &  ! Eix-Hix are 4 coefs of photo-ioniz cross-section of high-Z dopant, see "Inz.inp"
     &   Fix(:,:), &
     &   Gix(:,:), &
     &   Hix(:,:), &
     &   Eth(:,:), &  ! State-to-state Ionization Threshold (for ioniz cross-secs)
     &    WI(:,:), &  ! single ionization probability (1/s), removal of one electron per e-impact
     &   WEX(:,:),  WDX(:,:), &  ! e-impact excitation & deexcitation probabilities (1/s)
     &   WRR(:,:),  WTB(:,:), &  ! radiative & 3-body recombination probabilities (1/s)
     &  WInd(:,:),  Wab(:,:), &  ! induced emission & Photo(BB)absorption probabs (1/s)
     &  WPhI(:,:), WiRR(:,:), &  ! photoIonization & induced RR probabs, 1/s
     &   WDC(:,:), &  ! probability of Dielectronic Capture into AI EL of He-like & Li-like ions
     &   EAI(:,:), &  ! FACn energy of AI transition (column "DE[eV]" in "AIw.inp)"
     &    PM(:,:), &  ! Probability Mtrx [1/s], t-Scale=10^40 for d02
     &  Wout(:), &  ! EL depletion rate, PM(k,k)= -Wout(k)
     &  WoutXE(:,:,:), &  ! "Wout" remembered for each XE for Stark
     &  POP(:), &  ! relative abundance of QSs
     &  POPi(:,:,:), &  ! ti-values of EL POP saved La,nX-dependent
     &  POPf(:,:,:), &  ! save as 'tf" value for loading in POPs in Scenario for next ti
     &  POPZ(:,:,:), &  ! relative abundance of ionization stages, including nucl, which has SS= HSS+1
     &  WEAF(:)  ! NAG params and WorkArea (Nwork)
      real(8), public, allocatable :: &
     &   Norm(:,:), &  ! (MNLe,LaMx) integral of the line shape: it becomes < 1 after hard wings cut & must be used for re-norm of the shapes to 1
     &   hvCh(:,:), &  ! (MNLe,nXE) Spectral line Centers before hv-regulation
     &   hvC(:), &  ! (MNLe)     Spectral line Centers after hv-regulation
     &   QMlimit(:,:), &  ! (MNLe,LaMx) lower limit for level width from uncertainty princip
     &   FWevLor(:,:)  ! (MNLe,LaMx) Lorentzian (Stark) part of FWHM [eV]

!     npInpRes (number of hv points in each InpResp*.inp) is counted in OpenFiles

!  ---- Channel lists of the atomic database of X (Nov 2026), built once by BuildChannels
!       (first call of Ws). For level k the channels are ChXxxS(k) ... ChXxxS(k+1)-1:
!       ChInzKf  ionization / recombination k -> kf (bra = 1),
!       ChExcKf  excitation k -> kf (fit given, same SpS, E(kf) > E(k)),
!       ChDcK    AI levels kAI coupled to k by autoionization / dielectronic capture.
      integer, public, allocatable :: ChInzS(:), ChInzKf(:), ChExcS(:), ChExcKf(:), &
     &                                ChDcS(:), ChDcK(:)

      contains

      SUBROUTINE BuildChannels()  ! channel lists of X (nX = 1), see ChInzS above
      integer k, kf, n, nI, nE, nD
      n = NST(1)
      allocate(ChInzS(n+1), ChExcS(n+1), ChDcS(n+1))
      nI = 0;  nE = 0;  nD = 0
      do k = 1, n                        ! count
        do kf = 1, n
          if(k .ne. Nnu(1) .and. bra(k,kf,1) .ge. 0.5) nI = nI + 1
          if(MthdEX(k,kf) .ne. -7 .and. KiSS(kf,1) .eq. KiSS(k,1) .and. &
     &       E(kf,1) - E(k,1) .gt. zero) nE = nE + 1
        enddo
        do kf = Nnu(1)+1, n
          if(WAiz(kf,k,1) .ge. 10.) nD = nD + 1
        enddo
      enddo
      allocate(ChInzKf(max(nI,1)), ChExcKf(max(nE,1)), ChDcK(max(nD,1)))
      nI = 0;  nE = 0;  nD = 0
      do k = 1, n                        ! fill, in the order of k, kf as the old loops
        ChInzS(k) = nI + 1
        ChExcS(k) = nE + 1
        ChDcS(k)  = nD + 1
        do kf = 1, n
          if(k .ne. Nnu(1) .and. bra(k,kf,1) .ge. 0.5) then
            nI = nI + 1;  ChInzKf(nI) = kf
          endif
          if(MthdEX(k,kf) .ne. -7 .and. KiSS(kf,1) .eq. KiSS(k,1) .and. &
     &       E(kf,1) - E(k,1) .gt. zero) then
            nE = nE + 1;  ChExcKf(nE) = kf
          endif
        enddo
        do kf = Nnu(1)+1, n
          if(WAiz(kf,k,1) .ge. 10.) then
            nD = nD + 1;  ChDcK(nD) = kf
          endif
        enddo
      enddo
      ChInzS(n+1) = nI + 1
      ChExcS(n+1) = nE + 1
      ChDcS(n+1)  = nD + 1
      END SUBROUTINE BuildChannels

      SUBROUTINE AllocArrays()  ! Generic version: sizes of atomic-data arrays at run time.
!  Reads lines 1-4 of Params0.inp (FSS, HSS, Nnu, NST of the nXE elements) on its own unit;
!  Intro() reads the same lines again from unit 12 as before, so Params0.inp is unchanged.
      integer iu, ios, w4(nXE), mHSS(nXE), mNST(nXE)
      real(8) GB
      open(newunit=iu, file='Params0.inp', status='old', iostat=ios)
      if(ios.ne.0) STOP 'AllocArrays: cannot open Params0.inp'
      read(iu,*) w4  ! FSS
      read(iu,*) mHSS  ! HSS
      read(iu,*) w4  ! Nnu
      read(iu,*) mNST  ! NST
      close(iu)
      if(maxval(mHSS).lt.1 .or. maxval(mNST).lt.2) &
     &   STOP 'AllocArrays: bad HSS/NST'
      GB = (44.5d0*dble(maxval(mNST))**2 &
     &      + 30.d0*maxval(mNST)*nXE*LaMx)*8.d0/1.d9
      write(*,'(a,i6,a,i4,a,f6.2,a)') ' Params0.inp: nQSm = max(NST) =', &
     &   maxval(mNST), ',  HSSm = max(HSS) =', maxval(mHSS), &
     &   ';  arrays ~', GB, ' GB'
      CALL AllocSized(maxval(mNST), maxval(mHSS))
      END SUBROUTINE AllocArrays

      SUBROUTINE AllocSized(nQSmIn, HSSmIn)  ! allocate and zero all arrays of
      integer nQSmIn, HSSmIn  ! size nQSm, HSSm (also used by tests)
      nQSm = nQSmIn
      HSSm = HSSmIn
      Nwork= 50+ 12*nQSm+ nQSm**2

      allocate(QSname(nQSm,nXE))
      QSname = ' '
      allocate(nuAS(HSSm,nXE), nuQS(HSSm,nXE), nuGS(HSSm+1,nXE), &
     &   kAI1(HSSm,nXE), kAI2(HSSm,nXE), kiSS(nQSm,nXE), pqn(nQSm,nXE), &
     &   nu1(nQSm), nu2(nQSm), eqEL(nQSm,nXE), orb(nQSm,nXE), &
     &   MthdEX(nQSm,nQSm))
      nuAS= 0;  nuQS= 0;  nuGS= 0;  kAI1= 0;  kAI2= 0;  kiSS= 0;  pqn= 0
      nu1= 0;   nu2= 0;   eqEL= 0;  orb= 0;   MthdEX= 0

      allocate(LvJW(LaMx,nXE,nQSm), LvLorW(LaMx,nXE,nQSm), &
     &   PI(HSSm,nXE), PIR(HSSm,nXE,LaMx), DPI(HSSm,nXE,LaMx), &
     &   E(nQSm,nXE), g0(nQSm,nXE), BE(nQSm,nXE,LaMx), &
     &   BEp(nQSm,nXE,LaMx), bra(nQSm,nQSm,nXE), A(nQSm,nQSm,nXE), &
     &   WAiz(nQSm,nQSm,nXE), flu(nQSm,nQSm,nXE))
      LvJW= zero;  LvLorW= zero;  PI= zero;   PIR= zero;  DPI= zero
      E= zero;     g0= zero;      BE= zero;   BEp= zero;  bra= zero
      A= zero;     WAiz= zero;    flu= zero

      allocate(Ax(nQSm,nQSm), Bx(nQSm,nQSm), Cx(nQSm,nQSm), &
     &   Dx(nQSm,nQSm), Ex(nQSm,nQSm), Fx(nQSm,nQSm), &
     &   Aix(nQSm,nQSm), Bix(nQSm,nQSm), Cix(nQSm,nQSm), Dix(nQSm,nQSm), &
     &   Eix(nQSm,nQSm), Fix(nQSm,nQSm), Gix(nQSm,nQSm), Hix(nQSm,nQSm), &
     &   Eth(nQSm,nQSm))
      Ax= zero;  Bx= zero;  Cx= zero;  Dx= zero;  Ex= zero;  Fx= zero
      Aix= zero; Bix= zero; Cix= zero; Dix= zero
      Eix= zero; Fix= zero; Gix= zero; Hix= zero; Eth= zero

      allocate(WI(nQSm,nQSm), WEX(nQSm,nQSm), WDX(nQSm,nQSm), &
     &   WRR(nQSm,nQSm), WTB(nQSm,nQSm), WInd(nQSm,nQSm), &
     &   Wab(nQSm,nQSm), WPhI(nQSm,nQSm), WiRR(nQSm,nQSm), &
     &   WDC(nQSm,nQSm), EAI(nQSm,nQSm), PM(nQSm,nQSm))
      WI= zero;   WEX= zero;  WDX= zero;  WRR= zero;  WTB= zero
      WInd= zero; Wab= zero;  WPhI= zero; WiRR= zero; WDC= zero
      EAI= zero;  PM= zero

      allocate(Wout(nQSm), WoutXE(nQSm,nXE,LaMx), POP(nQSm), &
     &   POPi(nQSm,nXE,LaMx), POPf(nQSm,nXE,LaMx), &
     &   POPZ(HSSm+1,nXE,LaMx), WEAF(Nwork))
      Wout= zero;  WoutXE= zero;  POP= zero;  POPi= zero;  POPf= zero
      POPZ= zero;  WEAF= zero
      END SUBROUTINE AllocSized

      SUBROUTINE AllocLines(nLines)  ! Generic version: MNLe = number
      integer nLines  ! of spectral lines + 1 (LineList)
      MNLe = nLines + 1
      allocate(nX3(MNLe), nUp(MNLe), nLo(MNLe), &
     &   nUph(MNLe,nXE), nLoh(MNLe,nXE), &
     &   Norm(MNLe,LaMx), hvCh(MNLe,nXE), hvC(MNLe), &
     &   QMlimit(MNLe,LaMx), FWevLor(MNLe,LaMx))
      nX3= 0;  nUp= 0;  nLo= 0;  nUph= 0;  nLoh= 0
      Norm= zero;  hvCh= zero;  hvC= zero;  QMlimit= zero
      FWevLor= zero
      END SUBROUTINE AllocLines

      end module co2_atom

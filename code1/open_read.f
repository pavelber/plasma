      SUBROUTINE OpenFiles()   ! List spectral lines to be observed in [hvSmo, hvMax] and Print hv-regulated LineList in file #30.
      use mo1
      implicit none
      open( 13,file='Flag.inp')
      open(111,file='QSsKR272.inp')
      open(112,file='ExcKR272.inp')
      open(113,file='InzKR272.inp')
      open(114,file='AIwKR272.inp')

      open(211,file='QSsMG272.inp')
      open(212,file='ExcMG272.inp')
      open(213,file='InzMG272.inp')
      open(214,file='AIwMG272.inp')

      open(41, file= 'Comment.dat')

      open(42,file='PCDrespo8umBe1umCH.inp')  ! response of PCD with Be filter (absorption of 0.5 mm diamond * transmission of 8mcm Be + 1 mm CH filter) Oct 19, 2011
      read(42,'(a10)') title
      do iv = 1, npBe8
        read(42,*) hv8Be(iv), RespoBe(iv)     ! photon energy and response
      enddo
      close(42)

      open(51,file='Respo5milKapPCD.inp')     ! response of hard PCD with 5 mils kapton filter  [1mil = 25.4 mcm; mils= mInch]
      read(51,'(a10)') title
      do iv = 1, np5kap
        read(51,*) hv5kap(iv), Resp5kap(iv)   ! photon energy and response
      enddo
      close(51)

      open(52,file='Respo10miKapPCD.inp')     ! response of PCD with 10 mils kapton filter
      read(52,'(a10)') title
      do iv = 1, np10ka
        read(52,*) hv10ka(iv), Resp10ka(iv)   ! photon energy and response
      enddo
      close(52)

      open(53,file='Respo40miKapPCD.inp')     ! response of PCD with 40 mils kapton filter
      read(53,'(a10)') title
      do iv = 1, np40ka
        read(53,*) hv40ka(iv), Resp40ka(iv)   ! photon energy and response
      enddo
      close(53)

      open(55,file= 'TX_1520_respo1.inp')      ! TREX response in 1st order of mica reflection
      read(55,'(a10)') title
      do i = 1, npTX
         read(55,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(1,i)   !  hv point [keV], .... , response [arb.u.]
      enddo
      close(55)

      open(56,file= 'TX_1520_respo2.inp')      ! TREX response in 2nd order of mica reflection
      read(56,'(a10)') title
      do i = 1, npTX
         read(56,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(2,i)   !  hv point [keV], .... , response [arb.u.]
      enddo
      close(56)

      open(57,file= 'TX_1520_respo3.inp')      ! TREX response in 3rd order of mica reflection
      read(57,'(a10)') title
      do i = 1, npTX
         read(57,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(3,i)   !  hv point [keV], .... , response [arb.u.]
      enddo
      close(57)

      open(58,file= 'TX_1520_respo4.inp')      ! TREX response in 4th order of mica reflection
      read(58,'(a10)') title
      do i = 1, npTX
         read(58,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(4,i)   !  hv point [keV], .... , response [arb.u.]
      enddo
      close(58)

      open(59,file= 'TX_1520_respo5.inp')      ! TREX response in 5th order of mica reflection
      read(59,'(a10)') title
      do i = 1, npTX
         read(59,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(5,i)   !  hv point [keV], .... , response [arb.u.]
         hvTX(i)= 1.d3*hvTX(i)                                 !  [keV] to [eV] for use in computations
      enddo
      close(59)

      open(117, file='ZPops_BS.dat')  ! BS (La=2)
         write(117,'(a154)')  'tfNS      AlBe       AlLi       AlHe
     +  AlH        ALnucl     MgBe       MgLi       MgHe       MgH
     +  MGnucl     Dmm    TeEV     DenI     TiDkeV'

       open(21, file='POPs_BS_AL.dat')                                   ! POPs(t) of chosen six AL levels, numbers according to "QSsAL272.inp"
      write(21,'(a76)') 'tfNS   TeKeV       A         B         C        ! Chosen Level## shown in "FLAG.inp"
     +   D          E          F'

       open(22, file='POPs_BS_MG.dat')                                   ! POPs(t) of chosen six MG levels, numbers according to "QSsMG272.inp"
      write(22,'(a76)') 'tfNS   TeKeV       A         B         C        ! Chosen Level## shown in "FLAG.inp"
     +   D          E          F'

       open(32, file= 'MHD_BS.dat')
      write(32,'(a77)') 'tfNS   Dmm   TeEV   TiEV  TDkeV    niAL      ni
     +MG    ZbarAL  ZbarMG    Dene'

      open(46, file= 'LineWidth.dat')

      open(61, file= 'FuFluxFr1.dat')
      open(62, file= 'FuFluxFr2.dat')  ! FULL-hv-RANGE Radiation Flux [W/eV/sr] from 1 plasma sphere
      open(63, file= 'FuFluxFr3.dat')
      open(64, file= 'FuFluxFr4.dat')
      open(65, file= 'FuFluxFr5.dat')
      open(66, file= 'FuFluxFr6.dat')

      open(171, file= 'Fr1.dat')   !  TREX scans
      open(172, file= 'Fr2.dat')   !
      open(173, file= 'Fr3.dat')   !
      open(174, file= 'Fr4.dat')   !
      open(175, file= 'Fr5.dat')   !
      open(176, file= 'Fr6.dat')   !

      open (452, file= 'EffSpInK13inPrintFr.dat')
      write(452,'(a32)')  'hvEV       EffSpIn     kapR'

      open (100,file='Powers_TW_From_1_BS.dat')
      write(100,'(a90)') 'tfNS  FullPowTW    PwBeTW    Pow5mTW    Pow10T
     +W    Pow40TW    RadYieJ     TeKeV      ni'

      Return
      END



      SUBROUTINE Intro()  ! Read input files, print list of ELs, provide ELs p.q.n., A coefs and initial POPs; print this info in "Comment.dat"
      use mo1
      implicit none
      character*1 po1     ! for conversion of QSname2 symbol (1 position) in p.q.n.
      integer char2int    ! symbol-to-integer convertor function
      integer nSS, mth, LL, LU, iSS1, iEL1, iSS2, iEL2, ki, nFi
      real(8) Axw, Bxw, Cxw, Dxw, Exw, Fxw, Gxw, Hxw,
     +        fw, ONEplusD, AIw, trEn, thre

      flu = zero  ! correct-sign (>0) Absorption Oscillator Strength
      fVR = zero  ! flu(kL,kU,nX) or neighbour's 'flu' for Van-Regemorter excitation cross-section
      A   = zero  ! Einstein A coef
      WAiz= zero  ! AutoIoniz Prob from "AIwXE...inp"
      Ax  = zero  ! Ax to Fx are 2D coefs of the e-impact Exc Cross-Sec
      Bx  = zero
      Cx  = zero
      Dx  = zero
      Ex  = zero
      Aix = zero  ! Aix-Dix are 4 coefs of e-impact ioniz cross-section, see "InzXE....inp"
      Bix = zero
      Cix = zero
      Dix = zero
      Eix = zero  ! Eix-Hix are 4 coefs of photo-ioniz cross-section, see "InzXE....inp"
      Fix = zero
      Gix = zero
      Hix = zero
      Eth = -13.   ! State-to-state Ionization Threshold (for ioniz cross-secs)

      bra = zero   ! Preliminary. It will be changed to 1 when "Inz.inp" shows inz channel. Used in kinetics and BFFB emi/abso,
      MthdEX = -7  ! "-7" means "The code found excitation transition missing in "ExcXE....inp".
      kiSS = -66   ! for later check
      nuGS = 0
      kAI1 = 0     ! impo; used for compar (see 11 lines below); there will be "i-1" mistake, if SS ranges down to atom (FSS=1)

c   Read all "ELsXE....inp" files.
      do nX= 1, nXE
        nFi= 11 + 100*nX        ! file## == 111 and 211 are "ELsXE...inp" in Chin-type bases for AL and MG
        do k1 = 1, 2
          read(nFi,'(a9)') empty  ! 15 lines of comments 7 titles
        enddo
        nuGS(FSS(nX),nX)= 1     ! GS of First included SS of this XE is given #1 in the EL list of this XE
        LastAI(nX)= Nnu(nX)     ! preliminary
        do i= FSS(nX), HSS(nX)  ! all SSs of nX, except for NUCL
          read(nFi,*) nSS, nuQS(i,nX), nuAS(i,nX), PI(i,nX)
          if(nSS.ne.i) then
             write(*,'(a30, 2i3)') 'Inconsistency for XE#, SS=', nX, nSS
             write(*,'(a30, 2i3)') 'i, FSS(nX)=', i, FSS(nX)
		   STOP 'inconsistency #1'
          endif
          if(i.gt.FSS(nX)) nuGS(i,nX)= nuGS(i-1,nX)+ nuQS(i-1,nX)         ! this GS #  in the EL list of XE
          if(nuAS(i,nX).ne.0) then
            if(kAI1(i-1,nX).eq.0) kAI1(i,nX)= Nnu(nX) +1                  ! 1st AIEL has # = nucl# +1
            if(kAI1(i-1,nX).gt.0) kAI1(i,nX)= kAI1(i-1,nX) +nuAS(i-1,nX)  ! EL# of the 1st  AIEL of SS=i
            kAI2(i,nX)= kAI1(i,nX) + nuAS(i,nX) -1                        ! EL# of the last AIEL of SS=i
            LastAI(nX)= kAI2(i,nX)
          endif
        enddo
        nuGS(HSS(nX)+1, nX)= nuGS(HSS(nX),nX)+ nuQS(HSS(nX),nX)           ! nucl
        if(nuGS(HSS(nX)+1, nX).ne.Nnu(nX)) STOP 'Inconsistency #2'

  2     read(nFi,*) iSS
        do k= nuGS(iSS,nX), nuGS(iSS+1,nX)-1                ! all non-AI ELs of this SS
          read(nFi,'(a5,a5, f3.0, 2f11.3)') QSname1(k,nX),
     +      QSname2(k,nX), g0(k,nX), E(k,nX)
          kiSS(k,nX)= iSS
        enddo
        if(iSS.LT.HSS(nX)) goto 2

        read(nFi,*) nSS         ! nucl SS
        read(nFi,'(a9)') empty  ! "Nucl."
        g0(Nnu(nX),nX)= one
        E(Nnu(nX),nX)= zero
        QSname1(Nnu(nX),nX)= 'nucl.'  ! character*5
        QSname2(Nnu(nX),nX)= '     '  ! character*5
        kiSS(Nnu(nX),nX)= HSS(nX)+1

*  read AI EL names and info and prescribe "kiSS(EL,nX)"
        do j= FSS(nX), HSS(nX)
          if(nuAS(j,nX).lt.1) cycle
          read(nFi,'(a9)') title
          do k = kAI1(j,nX), kAI2(j,nX)
            read(nFi,'(a5,a5, f3.0, 2f11.3)') QSname1(k,nX),
     +           QSname2(k,nX), g0(k,nX), E(k,nX)
            kiSS(k,nX)= j
          enddo
        enddo

        do k = 1, LastAI(nX)          ! all non-AI and AI EL# of this XE
          if(k.eq.Nnu(nX)) cycle      ! skip nucl
          po1 = QSname2(k,nX)(2:2)    ! (2:2) means "positions from 2 until 2"; it must be column #7, which contains p.q.n. of outer electron
          nprin(k,nX)= char2int(po1)  ! p.q.n. of outer electron
        enddo

Check Yes/No equal eigenvalues E(k,nX) in the base. If YES, increase 2nd energy by 0.001 eV to avoid DE=0 in the computations
        do k = 1, LastAI(nX)-1
        do k1= k+1, LastAI(nX)
          if(KiSS(k,nX) .ne. KiSS(k1,nX)) cycle
          if(E(k,nX) .eq. E(k1,nX)) then
		   write(*,'(a16, f10.3, a14, i2,2i4)')
     +         'Equal energies=', E(k1,nX), 'for XE k k1=', nX, k, k1
             E(k1,nX)= E(k1,nX) +0.001
             write(*,'(a20, f12.3)') 'Changed E(k1) to', E(k1,nX)
          endif
        enddo
        enddo
      enddo   ! relates to nX-loop of reading/processing "ELsXE.inp"

c  Read excitation cross sections and f's from all 'ExcXE....inp' files.  Note: last line of "ExcXE...inp" MUST corresp to Nnu-2 --> Nnu-1
      do nX= 1, nXE
        nFi= 12+ 100*nX             ! "ExcXE.inp" file## == 112, 212, 312, 412
        read(nFi,'(a9)') title
        read(nFi,'(a9)') empty
  6     read(nFi,*) iSS, LL, LU, mth, Axw, Bxw, Cxw, Dxw, Exw, Fxw, fw

        if(LL.lt.nFAI) kL= nuGS(iSS,nX)-1+LL       ! order# in XE total EL list for non-AI EL
        if(LU.lt.nFAI) kU= nuGS(iSS,nX)-1+LU
        if(LL.ge.nFAI) kL= kAI1(iSS,nX) + LL-nFAI  ! order# in XE total EL list for AI EL
        if(LU.ge.nFAI) kU= kAI1(iSS,nX) + LU-nFAI
        if(abs(Fxw).gt.1.d-30 .AND. mth.ne.16) then
            write(*,*) 'OPEN Fx array: ISS=', ISS, 'LL=', LL, 'LU=', LU
            STOP 'OPEN "Fx" array'
        endif

        DE= E(kU,nX) - E(kL,nX)
        if(DE .le. zero) then
            write(*,'(7i8,2f12.5)')
     +        iSS, nFAI, LL, LU, kL,kU, nX, E(kL,nX), E(kU,nX)
            STOP 'Excitation down in reading Exc...inp'
        endif

        MthdEX(kL,kU,nX)= mth

        Ax(kL,kU,nX)= Axw
        Bx(kL,kU,nX)= Bxw
        Cx(kL,kU,nX)= Cxw
        Dx(kL,kU,nX)= Dxw
        Ex(kL,kU,nX)= Exw
c       Fx(kL,kU,nX)= Fxw     ! 5th Excit coef not used in Methods #5 and #11
        if(fw.LT.0.) then     ! "-" sign marks true transition, according to FAC.
          flu(kL,kU,nX)= -fw  ! correct (positive) value of Absorption Oscillator Strength
          fVR(kL,kU,nX)= -fw  ! for Van-Regemorter formula, if someone wants to use it by choosing MthdEX(kL,kU,nX)= 0
        endif
        if(fw.GT.0. .and. mth.eq.0) fVR(kL,kU,nX) = fw               !  fw > 0 is stored for Van-Regemorter formula only; f and A== 0
        if(fw.GT.0. .and. mth.ne.0) STOP 'found f>0 for non-VR method'
        A(kU,kL,nX)= 4.3450d7* flu(kL,kU,nX) *g0(kL,nX)
     +               *(E(kU,nX)-E(kL,nX))**2 /g0(kU,nX)

        if(iSS.eq.HSS(nX) .and. kL.eq.Nnu(nX)-4 .and.                    ! Pavel's change -2 to -4 to make it run for Kr
     +                          kU.eq.Nnu(nX)-1) goto 7                  ! It must be the last line of "ExcXE.inp"
        if(mth.eq.0  .or. mth.eq.5 .or. mth.eq.11 .or. mth.eq.16) goto 6 ! "-5" is "5 of low accyracy"; Lenya

        write(*,'(a65,5i5)') 'Excit CrosSec fit-method is unknown for XE,
     +  iSS, LL, LU, mth=', nX, iSS, LL, LU, mth
        STOP
  7     close(nFi)
      enddo        ! for XE-loop, which reads "ExcXE.inp" files

      read(13,*) hvMin  ! minimal photon energy hv(eV) on the hv axis
      read(13,*) hvSmo  ! edge of [hvMin,hvSmo] domain, where lines have life-time FWHM > 1 eV, thus merge and do not need fine resolution
      read(13,*) hvMiF  ! First hv-point of superFine interval of hv
      read(13,*) hvMaF  ! Last  hv-point of superFine interval of hv
      read(13,*) hvMax

************************  pose hv points along hv axis
      hvV(1)= hvMin
      ONEplusD= (hvSmo/hvMin) ** (one/99)     ! 1+delta
      do iv= 2, 100
        hvV(iv)= hvV(iv-1)*ONEplusD
      enddo

      ONEplusD= (hvMiF/hvSmo) ** (1./((nvM-300.)/2.-1.))
      do iv= 101,  99+(nvM-300)/2
        hvV(iv)= hvV(iv-1)*ONEplusD
      enddo

      ONEplusD= (hvMaF/hvMiF) ** (1./(nvM/2.-150.))
      do iv= 100+(nvM-300)/2 , nvM-200
        hvV(iv)= hvV(iv-1)*ONEplusD
      enddo

      ONEplusD= (hvMax/hvMaF)**(one/200)
      do iv= nvM-199, nvM
        hvV(iv)= hvV(iv-1)*ONEplusD
      enddo

 	read(13,*) fluMin   ! Min allowed value of OscStrnght.  If f() < fluMin, we replace A by 0 in "Intro"
 	read(13,*) AulMin   ! Min allowed value of Eins A coef. If A() < AulMin, we replace A by 0 in "Intro"
      read(13,*) empty

c  Exclude weak lines and lines on the edges of hv interval
      do nX= 1, nXE
      do k  = 1, NST(nX)  ! L-state: all ELs
      do k2 = 2, NST(nX)  ! U-state: all ELs
         if(flu(k, k2,nX) .LT. fluMin .or.     ! if  f < fluMin : kill this line for shorter computation time
     +        A(k2,k ,nX) .LT. AuLMin .or.     ! if  A < AulMin : kill this line for ..........
     +       (E(k2,nX)-E(k,nX)).LT. hvSmo)     ! if in low-resolution part of hv axis. I excluded lines at hv < hvSmo because they are weak vs free-free continuum.
     +        A(k2, k, nX) = zero              ! it will exclude the line from the LineList, thus from the computation.
      enddo
      enddo
      enddo

c  Read AI Probabilities from 'AIwXE....inp' file
      do nX= 1, nXE
        nFi= 14+ 100*nX        ! "AIwXE....inp"  files are ## == 114, 214, 314, 414
        read(nFi,'(a9)') title
  8     read(nFi,*) iSS1, iEL1, iSS2, iEL2, AIw, trEn  ! the energy "trEn" is for consistency control only
        if(iEL1.lt.nFAI) STOP 'non-AI initial state in "AIw...inp"'
        if(iEL2.ge.nFAI) STOP 'AI final state in "AIw...inp"'
        if(iSS1.eq.111) goto 402                                    ! "111" shows stop-reading line: file end.  MY contribution.

        ki= kAI1(iSS1,nX)+ iEL1-nFAI  ! order# in XE total EL list for AI initial EL
        kf= nuGS(iSS2,nX)-1 +iEL2     ! order# in XE total EL list for non-AI final EL
Consistancy control 1:
        if(kiSS(kf,nX) .ne. kiSS(ki,nX)+1) then
	     write(*,*) 'XE, ki, kf =', nX, ki, kf
           STOP 'have error in "AIaXE...inp" level numbers'
        endif
Consistancy control 2: AI transition energy DE == E(qAI) - [PIR + E(fin)], i.e. E(i) - E(f), where both E taken
c                      relative to common "0", which is GS of "2ex ion".  Here we check consistency of DataBases:
c                      compare "trEn" from "AIwXE...inp"  to  E(qAI)-[PI+E(fin)] from "ELsXE....inp".
        if(abs(trEn-(E(ki,nX)-PI(iSS1,nX)-E(kf,nX))) .gt. 2.0) then  ! Changed to 2.0 for Kr Pavel
           write(*,'(a25)') 'Inconsistency in AI transition energy:'
           write(41,'(i2, 6i4, 6e12.6)') nX, ki, iSS1, iEL1,
     +       kf, iSS2, iEL2, trEn, E(ki,nX)-PI(iSS1,nX)-E(kf,nX),
     +      E(ki,nX), PI(iSS1,nX), E(kf,nX)
	    STOP
        endif
        WAiz(ki,kf,nX)= AIw
        goto 8
 402    close(nFi)
      enddo       ! nX loop for AI

****** Read the ionization cross-secs coefs from "InzXE....inp" and assign 1/0 to "bra(i,f,XE)" based on Yes/No ioniz channel in "InzXE...inp"
      do nX= 1, nXE
        nFi= 13+ 100*nX         ! "InzXE....inp"  files are ## == 113, 213, 313, 413
        read(nFi,'(a9)') comme
        read(nFi,'(a9)') title
  9     read(nFi,*) iSS1, iEL1, iSS2, iEL2, Axw, Bxw, Cxw,
     +              Dxw, mth, Exw, Fxw, Gxw, Hxw, thre     ! "thre" is ioniz threshold
        if(iSS1.eq.111) goto 405                           ! "111" is a sign of stop-reading (technical) line that I added
        if(iEL1.lt.nFAI) ki= nuGS(iSS1,nX)-1 +iEL1         ! order# in XE total EL list for non-AI EL
        if(iEL1.ge.nFAI) ki= kAI1(iSS1,nX)   +iEL1-nFAI
        if(iEL2.lt.nFAI) kf= nuGS(iSS2,nX)-1 +iEL2         ! order# in XE total EL list for non-AI EL
        if(iEL2.ge.nFAI) kf= kAI1(iSS2,nX)   +iEL2-nFAI
        if(kiSS(kf,nX) .ne. kiSS(ki,nX)+1) then
	     write(*,*) 'XE, ki, kf =', nX, ki, kf
           STOP 'have error in "InzXE...inp" level numbers'
        endif
        if(mth.ne.4) then
	     write(*,*) 'XE, ki, kf =', nX, ki, kf
           STOP 'in "InzXE.inp" e-imp cross-sec formula# ne 4: no expre'
        endif
        Aix(ki,kf,nX)= Axw  ! Aix-Dix are 4 coefs of e-impact ioniz cross-section (formula #4), see "InzXE....inp"
        Bix(ki,kf,nX)= Bxw
        Cix(ki,kf,nX)= Cxw
        Dix(ki,kf,nX)= Dxw
        Eix(ki,kf,nX)= Exw  ! Eix-Hix are 4 coefs of photo-ioniz cross-section, see "InzXE....inp"
        Fix(ki,kf,nX)= Fxw
        Gix(ki,kf,nX)= Gxw
        Hix(ki,kf,nX)= Hxw
        Eth(ki,kf,nX)= thre ! State-to-state non-reduced Ionization Threshold [for single-e ioniz cross-secs; it must be reduced with DPI(iSS1,nX)]
        bra(ki,kf,nX)= one  ! "one" means yes single-e ionization channel between ki and kf. With FAC-bases "bra" is either 1 or 0
        goto 9
 405    close(nFi)
      enddo        ! nX loop for Ioniz cross-sec

c  Read 'Flag.inp'
      do nX= 1, nXE
        read(13,*) kQS      ! EL# for initial-POP-load
        do La = 1, LaMx
          do k = 1, NST(nX)
            if(k.ne.kQS) POPt(k,nX,La)= 1.d-10
            if(k.eq.kQS) POPt(k,nX,La)= 1.d0 - (NST(nX)-1)*1.d-10
          enddo
        enddo
      enddo

      do nX= 1, nXE
        read(13,*) AtMass(nX)               ! ion mass [a.u.] for Doppler linewidth calcul
        Imas(nX) = AtMass(nX) *1.66054d-24  ! ion mass [g]
      enddo

      TX1 = hvTX(2)        ! 2nd point of TREX response in 5 orders
      TX2 = hvTX(npTX-1)   ! pre-last
      do iv = 1, nvM
        if(hvV(iv) .lt. TX1) iTX1= iv+1  ! # of 1st  v-point within TREX response interval
      enddo
      do iv = 1, nvM
        if(hvV(iv) .lt. TX2) iTX2= iv	   ! # of last v-point within TREX response interval
      enddo

      read(13,*) KeRedu  ! -1 means "no reduction", 0 means "IonSph"
      read(13,'(a9)') empty
      read(13,'(a9)') empty
      read(13,*) strt, Dstrt, TeStrt, TDstrt, nStrt, peMGst   ! initial values [cm, eV, keV, i/cc   %]
	do i = 1, mSpe
         read(13,*) tres(i), Dfr(i), TeFr(i), TDfr(i), nFr(i), peMG(i)  ! s, cm, eV, keV, i/cc   perc
      enddo
      read(13,'(a9)') empty

      read(13,*) npa        ! umber of t-points in (tres(j), tres(j+1)] interval. This number includes t= tres(j+1) point
      read(13,'(a9)') empty
      read(13,'(a9)') empty

      read(13,*) PrFr       ! = 1,2,...6 is "tres(#)" for printing "PCDs_Resp_i_Pow_at_PrFr.dat", 'LineWidth.dat', 'EmiAbsoDetails.dat', ...
      read(13,'(a9)') empty

      read(13,*) PoLeNu(1,1), PoLeNu(1,2), PoLeNu(1,3), PoLeNu(1,4),     ! Numbers of 6 levels of Al chosen for printing their POP(t) in files "POPs_BS_AL.dat".
     +           PoLeNu(1,5), PoLeNu(1,6)
      read(13,*) PoLeNu(2,1), PoLeNu(2,2), PoLeNu(2,3), PoLeNu(2,4),     ! Numbers of 6 levels of Mg chosen for printing their POP(t) in files "POPs_BS_MG.dat".
     +           PoLeNu(2,5), PoLeNu(2,6)

      read(13,'(a9)') empty
      read(13,'(a9)') empty
      read(13,*) KeySmooth ! Key = 0 for broken-line interpo betw params at tres(#) points; see Scenario
*                                = 2 for smooth two-half-Lorentzians approximation of Te(t)
      read(13,*) tresPeak  ! # = 1,2, ...,6 in "tres(#)" chosen for the peak of Te(t)
      read(13,*) PeakTe    ! Peak value of Te(t)
      read(13,*) HWL1      ! HWHM of Lor Te(t) before the peak of Te(t)
      read(13,*) HWL2      ! HWHM of Lor Te(t) aafter the peak of Te(t)
      close(13)

      write(46,'(a63, i1, a70)')  'Info on Line Broadening (FWHM/hvC).
     + BS at Fr',  PrFr,  'time.  We omitted lines with PopU < 1.d-8 and
     + lines with hvC < hvSmo.'
      write(46,'(/a137)') 'hvCeV   XE  SpS  Lambda[A]      UPPER
     +  LOWER      GauTD   Jstark    Natur   Lorentz   Voigt   WoutUp
     +WoutLo    PopL     PopU'
      Return
      END	  ! of INTRO subr


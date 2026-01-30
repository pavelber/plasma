      use mo1co2nov        
      implicit none
      CALL OpenFiles() ! Open Output files, write titles of columns
      CALL Intro()     ! Read input files; fill in atomic data arrays and 
c                        initial population ("POPs") of energy levels ("ELs")
      CALL LineList()  ! Find spectral lines in full [hvmin, hvmax] interval, print file "LineList.dat"
      CALL hvPoints()  ! Build the array of hv points in full [hvMin, hvMax] interval 
      Count = 0        ! This integer counts lines during reading atomic data files
      FrYie = 0.		 ! FrYie(iv,frame#)= 0.  ! t-integral of SpPow(iv) [W/eV/sr] within j-th frame

      tf= strt
  1   ti= tf  
      tf= ti+ tstep  
      write(*,'(/a40, f6.2, a4)') 'Start ti =', ti*1.d12, 'ps:'

      if(ti.GE.StopTime) STOP '   ti >= StopTime.  The End.'

      do La = 1, LaMx     ! La is zone number. La=1 is BS, La=2 is Core, La=3 is Capsule     
        CALL SCENARIO()   ! for t= ti compute R, Te, u3D, DenI, ZC1, Dene
        write(*,'(a34, i2, e9.2, f7.0, e9.2, e11.4, f10.4)') 
     +          'Zone, R(cm), Te, u3D, ne, Z(X) =', 
     +           La, CeR(La), Te(La), u3D(La), Dene(La), ZC(1,La)  

        write(30+La,'(f7.2, e10.3, 2f7.3, e11.4, 2f10.5, 3f7.2, e11.4,   ! files "BSinfo.dat", "CoreInfo.dat", "CapInfo.dat"      
     +                 3e10.3, f6.3, 2f8.2)')  ti*1.d12,  CeR(La)*1.d4, 
     +                      Te(La)/1.d3, u3D(La)/1.d7, Dene(La), 
     +                 ZC1(La), ZC(1,La), ZC(2,La), ZC(3,La), ZC(4,La),  ! ZC(nX,La) is mean ion charge of (nX,La), computed in Scenario  
     +                      Den(1,La), Den(2,La), Den(3,La), Den(4,La),  ! using "POPi", see "POPi(k,nX,La) = POPf(k,nX,La)" just there
     +                                bp(La), bc(La)/1.d3, bw(La)/1.d3   ! [keV]

        do nX = 1, 1 ! nXE  ! For Te=Ti, Den(nX,La) of t = ti. Note: only X has lines and DPI 
           CALL LevWi()     ! For these La,nX compu FWHM [eV] of all E-LEVELs "LvJW(La,nX,k)" [eV] 

           CALL redPI()     ! For these La,nX, for each jSS compu DPI(jSS,nX,La) and PIR(jSS,nX,La).  

           CALL BEvPr()     ! For these La,nX compu Binding Energy of outermost electron "BE(k,nX,La)".
c                             Move POPs from BE-cut LEs to ground state of next SpS.

c                           Gaussian part of FWHM/hvC of lines of #nX in zone #La is denoted "FWkVcGAU(La,nX)"
           Amass = AtMass(nX) *Uam                     ! [g] mass of nX ion
           uTi   = sqrt(8.*BolEr*Tion(La)/pin/Amass)   ! [cm/s] mean thermal velocity of nX ion in La
           fwTiKvC(La,nX)= 1.4757*uTi/c                ! my page on thermal and isotropic 3D hydro broadenings                        
           fwTUKvC(La,nX)= 1.4757*u3D(La)/c
           FWkVcGAU(La,nX)= sqrt(fwTiKvC(La,nX)**2 +fwTUKvC(La,nX)**2)  ! FWHMgau/hvC of 2 convolved Gaussians.
        enddo  ! nX

        CALL EmiAbso()  ! Compu this-La "EmTot(La,hv)" [W/cc/sr/eV] and "AbTot(La,hv)" [1/cm]
c                         via params of t=ti and POPi(k,nX,La).  
      enddo  ! La loop;   La is zone number; La=1 is BS, La=2 is Core, La=3 is Capsule       

      CALL EffSpIn()  ! Compute due-to-all-La "SpInEf(La,iv)" [W/eV/sr/cm2] in RP of each "La", 
c                       La-loop is inside this subr because each "La" has contribs from other "La"s.  
c                      "SpInEf(La,iv)" is needed for Win, Wab, WphI, WiRR at t= ti" for d02 of POPs towards "tf".

      CALL PowYie()   ! Compute Spectral Power from the target using EmTot, AmTot computed
c	                  for params of t=ti and POPi(k,nX,La).  Print frames in files (181 - (186 and power detectors in files (187 - 189 	 
         nX = 1 
      do La = 1, LaMx	            
         If(Den(nX,La).gt.1.d10)  ! For each (nX,La) compute Ws & PM; run d02 for POPs from "ti" to "tf" with params of "ti".  
     +   CALL AtKins()            ! It gives "POPf(k,nX,La)" that is POP(tf) to be first used in Scenario of NEXT "ti" as POPi(k,nX,La).
      enddo   
      goto 1  ! for next t-step, where present "tf" will be "ti", thus present "POPf" will be used for computation of all quantities at "ti" 
      END     ! MAIN



      SUBROUTINE OpenFiles() 
      use mo1co2nov
      implicit none

      open(12, file= 'Params0.inp')
      open(13, file= 'Params1.inp')
      open(14, file= 'Params2.inp')

      open(211, file= 'QSsC.inp') 
      open(311, file= 'QSsHe.inp') 
      open(411, file= 'QSsD.inp') 

      open(111, file= 'QSs.inp') 
      open(112, file= 'Exc.inp')
      open(113, file= 'Inz.inp')
      open(114, file= 'AIw.inp')	

      open(31, file= 'BSinfo.dat')
      open(32, file= 'CoreInfo.dat')
      open(33, file= 'CapInfo.dat')
      do La= 1, LaMx
	   write(30+La,'(a146)')     'tiPS     Rmcm   TeKeV   u3D7    Dene
     +       Zbar       ZX       ZC    ZHe     ZH     DenX       DenC   
     +  DenHe      DenH     bp    bcKeV   bwKeV'  
      enddo

      open(46, file='BSsLineInfo.dat')
      open(47, file='CorLineInfo.dat')
      open(48, file='CapLineInfo.dat')
      do k7= 1, LaMx
        write(45+k7,'(a140)')      'Info on FWHM [eV] of spectral lines.
     +    Note: 0 in column "Baranger" means that upper level is cut by
     +continuum lowering.'
        write(45+k7,'(/a164)') 'hvCeV  ChE SpS  Lambda(A)   Upper        
     +      Upper                 Lower                Lower       viaTi
     +    via3D  Baranger  Lorentz   Voigt   PopUpper    flu'
      enddo

      open(49, file= 'Contacts.dat')
      open(61, file= 'EffSpIns.dat')    ! It displays [GW/KeV/sr/cm2] in representative point of each zone at t = "tiInf"

      open(116, file= 'ZPopsBS.dat')    ! population(t) of all ionization stages in BS
      open(117, file= 'ZPopsCore.dat')  !                                           Core
      open(118, file= 'ZPopsCap.dat')   !                                           Capsule
      do La= 1, LaMx
         write(115+La,'(/a114)')   'tfPS   TeKeV    Dene     DenX      P  ! printed just after "AtKins", thus t= "tf". After 
     +opzN     PopzC     PopzB     PopzBe    PopzLi    PopzHe    PopzH    ! this print the code goes to mark "1" and "tf" becomed "ti"   
     +  PopzNucl'                                                         
      enddo

      open(181, file= 'Frames.dat')   ! Specific yield [kJ/keV/sr] gained during each frame and during entier computation.
c                                       Spectral response of spectrometer_1 [arb.u.].  Spectrograms (spectral yield * response) [arb.u]  
      write(181,'(a216)') 'hvKeV      YieFr1     YieFr2     YieFr3     Y
     +ieFr4     YieFr5     YieFr6     YieFr7     YieFr8    tIntegY    Re
     +sponse   spectr1    spectr2	 spectr3    spectr4    spectr5    spec
     +tr6    spectr7    spectr8    SpectrY'

      open(185, file= 'Image1.dat')  ! 1 frame by 1st camera at ti = tiInf
      open(186, file= 'Image2.dat')  ! 1 frame by 2nd camera at ti = tiInf
      do k = 1, imgs                 ! all imaging cameras     
         write(184+k,'(a16)') 'rMCM    Image'	  ! norm to 1 at r=0
      enddo

      open(187, file= 'PowDet1.dat')  ! [arb.u.] t-dependent signal of Power Detector #1.  (former PCDs)
      open(188, file= 'PowDet2.dat')  ! [arb.u.] t-dependent signal of Power Detector #2
      open(189, file= 'PowDet3.dat')  ! [arb.u.] t-dependent signal of Power Detector #3
      do k = 1, 3
         write(186+k,'(a31)') 'timePS     PowTW     SignalArbU' 
      enddo

      open(191, file= 'FrameGauLor.dat')  ! One "Frame" [arb.u.] (III.3) convolved with Gaussian and Lorentzian.
      write(191,'(a42)') 'hvKeV      FrArbU    GauConv    LorConv'
	  
      open(201, file= 'InpResp1.inp')  ! on hv array of this file, spectral response of 1st spectrometer (full power from the target, "Frames.dat")
      open(202, file= 'InpResp2.inp')  ! on hv array of this file, spectral response of spectrometers on 1st "diagnostic" LoS, file "diFrames1.dat")
      open(203, file= 'InpResp3.inp')  ! on hv array of this file, spectral response of spectrometers on 2nd "diagnostic" LoS, file "diFrames2.dat")
      open(204, file= 'InpResp4.inp')  ! on hv array of this file, spectral response of spectrometers on 3th "diagnostic" LoS, file "diFrames3.dat")
      open(205, file= 'InpResp5.inp')  ! on hv array of this file, spectral response of camera which takes frames in 1st spectral interval. File "Images1.dat"
      open(206, file= 'InpResp6.inp')  ! on hv array of this file, spectral response of camera which takes frames in 2nd spectral interval. File "Images2.dat"
      open(207, file= 'InpResp7.inp')  ! on hv array of this file, spectral response of Power Detector #1. Used for "PowerDet1.dat"
      open(208, file= 'InpResp8.inp')  ! on hv array of this file, spectral response of Power Detector #2. Used for "PowerDet2.dat"
      open(209, file= 'InpResp9.inp')  ! on hv array of this file, spectral response of Power Detector #3. Used for "PowerDet3.dat"

      do j = 1, mDet      ! all detectors 
         read(200+j,'(a10)') title
         do iv = 1, npInpRes(j)
            read(200+j,*) hvRes(iv,j), InRes(iv,j)   ! input points of photon energy and response of j-th detector in this point 
         enddo	  		 	  
         close(200+j)
      enddo

      open(341, file= 'Happened.dat')
c      open(342, file= 'LorDev.dat')

      open(401, file= 'BSemiAbso.dat')    ! Emi and Abso details in BS
      open(402, file= 'COREemiAbso.dat')  ! Emi and Abso details in Core
      open(403, file= 'CAPemiAbso.dat')   ! Emi and Abso details in Capsule

      open(541, file='PIR_BS.dat')
      open(542, file='PIR_Core.dat')
      open(543, file='PIR_Cap.dat')
      do La= 1, LaMx
         write(540+La,'(/a60)') 'PIR/PI.  Not allowed < 0.3' 
         write(540+La,'(/a103)')  'tiPS   TeKeV    Dene     DenX      N-
     +like    C-like    B-like   Be-like   Li-like   He-like    H-like'
      enddo

      Return
      END     ! "OpenFiles"



      SUBROUTINE Intro()  ! Read input files, print list of ELs (non-AI and AI), provide ELs p.q.n., A coefs and initial POPs; print this info in "Comment.dat"  
      use mo1co2nov 
      implicit none
      character*1 po1     ! for conversion of EL name symbols (1 position) in p.q.n., Lorb, ...
      integer char2int                      ! symbol-to-integer convertor function
      integer iniQS, nSS, mth, LL, LU, ki,  
     +        iSS1, iQS1, iSS2, iQS2,  nFi                 
      real(8) fw, AIw, trEn, Axw, Bxw, Cxw, Dxw, Exw, Fxw,Gxw, Hxw, 
     +        thre, mePh  

      read(12,*) FSS(1), FSS(2), FSS(3), FSS(4)  ! "FSS" is serial # of 1st SS of X, C, He, D in database, e.g. 30 for N-like Kr
      read(12,*) HSS(1), HSS(2), HSS(3), HSS(4)  ! "HSS" is # of H -like SS of XX, C, He, D in the DaBa;  36 for H-like Kr
      read(12,*) Nnu(1), Nnu(2), Nnu(3), Nnu(4)  ! "Nnu" is serial number of nuclei in file "QSs.inp";   
      read(12,*) NST(1), NST(2), NST(3), NST(4)  ! "NST" is the number of QSs in DaBa of each chemical element   
      read(12,'(a9)') empty                      !  separating line  

c   Read "QSsXE.inp" files of C, He, D. 
      do nX= 2, nXE    !  C, He, D
        nFi= 11 + 100*nX   ! file## == 111, 211, ... are "QSsXE...inp" in Chin-type bases for XX,C,He, D  
        read(nFi,'(a9)') empty  ! 1st line of header  
        read(nFi,'(a9)') empty  ! 2nd line of header  

        nuGS(FSS(nX),nX)= 1     ! GS of First included SpS of this XE is given #1 in the EL list of this XE  
        do i= FSS(nX), HSS(nX)  ! all SpSs of nX, except its NUCL                
          read(nFi,*) nSS, nuQS(i,nX), nuAS(i,nX), PI(i,nX)
          if(nSS.ne.i) then
             write(*,'(a30, 2i3)') 'Inconsistency for XE#, SS=', nX, nSS
             write(*,'(a30, 2i3)') 'i, FSS(nX)=', i, FSS(nX)
             write(*,'(a50)') 'Re-Compile mo1co2nov.for'
		   PAUSE 'this is the inconsistency #1'
          endif
          if(i.gt.FSS(nX)) nuGS(i,nX)= nuGS(i-1,nX)+ nuQS(i-1,nX)     ! this GS #  in the EL list of XE  
        enddo 
        nuGS(HSS(nX)+1, nX)= nuGS(HSS(nX),nX)+ nuQS(HSS(nX),nX)       ! nucl     

        if(nuGS(HSS(nX)+1, nX) .ne. Nnu(nX)) PAUSE 'Inconsistency #2'

        read(nFi,'(a9)') empty  ! separating line   
  1     read(nFi,*) iSS 
        do k= nuGS(iSS,nX), nuGS(iSS+1,nX)-1   ! all non-AI ELs of this SpS   
          read(nFi,'(a24, f5.0, f13.3)') QSname(k,nX), g0(k,nX), E(k,nX)
          kiSS(k,nX)= iSS 
        enddo   
        if(iSS.LT.HSS(nX)) goto 1 

        read(nFi,*) nSS        ! nucl SS
        QSname(Nnu(nX),nX)= '         nucl.          '  ! character*24
        E(Nnu(nX),nX)= zero
        kiSS(Nnu(nX),nX)= HSS(nX)+1 
      enddo   ! nX = 2,3,4
      g0(Nnu(2),2) = 1.    ! C,  see comment  dated Nov 13, 2020    
      g0(Nnu(3),3) = 1.    ! He, 
      g0(Nnu(4),4) = 1.    ! D,      

      do nX= 2, nXE    !  C, He, D
         do k= 1, Nnu(nX) 
            if(k.eq.Nnu(nX)) goto 10   ! skip nucl
            po1 = QSname(k,nX)(22:22)  ! (22:22) means "positions from 14 until 14" which contains p.q.n. of outer electron 
            pqn(k,nX)= char2int(po1)   ! p.q.n. of outer electron

            po1 = QSname(k,nX)(23:23)  ! orbital q.n.(s,p,d,f...) of outer electron 
            orb(k,nX)= char2int(po1)   ! orbital q.n. = 0,1,2,3, ...

            po1 = QSname(k,nX)(24:24)  ! number of equivalent electrons in nl-subshell
            eqEL(k,nX)= char2int(po1)  
10          continue
         enddo
      enddo   
  
      SpInEf = zero  ! SpInEf(La,hv) is 4pi-average RF [W/cm^2/sr/eV] for Win, Wab, WphI and WiRR. ==0 is used in 1st-step
      SpeY   = zero  ! t-depe spectral yield [J/keV/sr] gathered since t0 until current "tf" 

      flu = zero  ! correct-sign (>0) Absorption Oscillator Strength 
      A   = zero  ! Einstein A coef 
      WAiz= zero  ! AutoIoniz Prob from "AIwXE...inp" 
      Ax  = zero  ! Ax to Fx are 2D coefs of the e-impact Exc Cross-Sec  
      Bx  = zero  
      Cx  = zero  
      Dx  = zero 
      Ex  = zero         
      Fx  = zero
      Aix = zero  ! Aix-Dix are 4 coefs of e-impact ioniz cross-section, see "InzXE....inp"
      Bix = zero  
      Cix = zero 
      Dix = zero
      Eix = zero  ! Eix-Hix are 4 coefs of photo-ioniz cross-section, see "InzXE....inp"
      Fix = zero
      Gix = zero
      Hix = zero 
      Eth = -13.  ! State-to-state Ionization Threshold (for ioniz cross-secs) 

      bra = zero  ! "bra(ki,kf,nX)"=0/1 is a marker of Yes/No ionization channel between two ELs.
c                   "1" will be given in "Intro" subr below.  
      MthdEX=-7   ! "-7" will mark excitation channels missing in "ExcXE....inp'.  

      kAI1= 0     
c   Read "QSs.inp" i.e. ELs of high-Z element (nX=1) 
      nX = 1
      nFi= 111                ! # of file  "QSs.inp"  
      read(nFi,'(a9)') empty  ! 1st line of header  
      read(nFi,'(a9)') empty  ! 2nd line of header  
      nuGS(FSS(1),1)= 1       ! GS of First included SS 
      do i= FSS(nX), HSS(nX)  ! all SSs of nX, except NUCL                
          read(111,*) nSS, nuQS(i,nX), nuAS(i,nX), PI(i,nX)
c          write(*,'(/a30, 3i5, f11.3)') 'nX, FSS, HSS, PI=', 
c     +                                   nX, FSS(nX), HSS(nX), PI(i,nX)
          if(nSS.ne.i) then
             write(*,'(a30, 2i3)') 'Inconsistency for XE#, SS=', nX, nSS
             write(*,'(a30, 2i3)') 'i, FSS(nX)=', i, FSS(nX)
             write(*,'(a50)')      'Re-Compile mo1co2nov.for'
		   PAUSE 'this is the inconsistency #1'
          endif
          if(i.gt.FSS(nX)) nuGS(i,nX)= nuGS(i-1,nX)+ nuQS(i-1,nX)         ! this GS #  in the EL list of XE  

          if(nuAS(i,nX).gt.0) then 
            if(kAI1(i-1,nX).eq.0) kAI1(i,nX)= Nnu(nX) +1                  ! 1st AI EL has # = nucl# +1
            if(kAI1(i-1,nX).gt.0) kAI1(i,nX)= kAI1(i-1,nX) +nuAS(i-1,nX)  ! EL# of the 1st  AI EL of SpS=i                 
            kAI2(i,nX)= kAI1(i,nX) + nuAS(i,nX) -1                        ! EL# of the last AI EL of SpS=i
            LastAI(nX)= kAI2(i,nX)                                        
          endif
      enddo 
      nuGS(HSS(nX)+1, nX)= nuGS(HSS(nX),nX)+ nuQS(HSS(nX),nX)       ! nucl     

      if(nuGS(HSS(nX)+1, nX) .ne. Nnu(nX)) PAUSE 'Inconsistency #2'

      read(nFi,'(a9)') empty  ! separating line   
  2   read(nFi,*) iSS 
      do k= nuGS(iSS,nX), nuGS(iSS+1,nX)-1    ! all non-AI ELs of "iSS"   
          read(nFi,'(a24, f5.0, f13.3, i6, i9 )') QSname(k,nX),  
     +                             g0(k,nX), E(k,nX), nu1(k), nu2(k) 
          kiSS(k,nX)= iSS 
          if(nX.eq.1) Count= Count +1   ! counter of lines read
c             write(*,'(i4,a24, f5.0, f13.3, i5)') iSS, QSname(k,nX), 
c     +                  g0(k,nX), E(k,nX), kiSS(k,nX)
      enddo   
      if(iSS.LT.HSS(nX)) goto 2

      read(nFi,*) nSS        ! nucl SS
      read(nFi,'(a9)') empty ! "Nucl." 
      QSname(Nnu(1),1)= '         nucl.          '  ! character*24
      g0(Nnu(1),1) = 1.      ! see comment  dated Nov 13, 2020 
       E(Nnu(1),1) = zero 
      nu1(Nnu(1))  = 1
      nu2(Nnu(1))  = Nnu(1) 

      Count= Count +1    ! counter of lines read, up to nucl (including nucl)
      if(Count.ne.Nnu(nX)) PAUSE 'XX nucl # =/= Nnu(1)'

      kiSS(Nnu(1),1)= HSS(1)+1 
	     
*  From "QSs.inp" read names of AI ELs and info. Prescribe "kiSS(EL,nX)"    ! For XX only, i.e. nX==1, see	~60 strings above
      nX = 1
      do j= FSS(nX), HSS(nX)-1        ! Until He-like SS (including them)
        if(nuAS(j,nX).lt.1) cycle
        read(nFi,'(a9)') title        ! info line
        do k = kAI1(j,nX), kAI2(j,nX)  
          read(nFi,'(a24, f5.0, f13.3, i6, i9 )') QSname(k,nX),  
     +                             g0(k,nX), E(k,nX), nu1(k), nu2(k) 
          kiSS(k,nX)= j 
          Count= Count +1      ! counter of lines read
        enddo 
      enddo
      close(111)  ! "QSs.inp"  

      if(Count .ne. NST(nX)) PAUSE 'Full Count =/= NST(1)'

      do k = 1, LastAI(nX)         ! all true and AI EL# of this XE
        if(k.eq.Nnu(nX)) goto 11   ! skip nucl
        po1 = QSname(k,nX)(22:22)  ! (22:22) means "positions from 14 until 14" which contains p.q.n. of outer electron 
        pqn(k,nX)= char2int(po1)   ! p.q.n. of outer electron

        po1 = QSname(k,nX)(23:23)  ! orbital q.n.(s,p,d,f...) of outer electron 
        orb(k,nX)= char2int(po1)   ! orbital q.n. = 0,1,2,3, ...

        po1 = QSname(k,nX)(24:24)  ! number of equivalent electrons in nl-subshell
        eqEL(k,nX)= char2int(po1)  
 11     continue  
      enddo

Check Yes/No equal E(k,nX) in DaBa. If YES, increase 2nd energy by 0.001 eV to avoid DE=0 in the computations
      do k = 1, LastAI(nX)-1
      do k1= k+1, LastAI(nX)
        if(KiSS(k,nX) .ne. KiSS(k1,nX)) cycle
          if(E(k,nX) .eq. E(k1,nX)) then
             write(341,'(/a16, f10.3, a14, i2,2i4)') 
     +         'Equal energies=', E(k1,nX), 'for XE k k1=', nX, k, k1
             E(k1,nX)= E(k1,nX)+ 0.001
             write(341,'(a20, f12.3)') 'Increased E(k1) to', E(k1,nX)
          endif		   
        enddo
      enddo

c  Read excitation cross sections and f's from 'Exc.inp'. 
      read(12,*) StrExc  ! From "Params0" read the number of strings in file "Exc.inp" (including the title)

      do nX= 1, 1            ! here X only
        nFi= 12+ 100*nX      ! "Exc.inp" file# == 112
        read(nFi,'(a9)') title
        read(nFi,'(a9)') empty  
        CountExc = 2
  6     read(nFi,*) iSS, LL, LU, mth, Axw, Bxw, Cxw, Dxw, Exw, Fxw, fw 
        CountExc = CountExc + 1

        if(LL.gt.0) kL= nuGS(iSS,nX) -1+LL   ! "kL" is order # of "LL" in "long" numbering. Here non-AI EL thus LL > 0           
        if(LU.gt.0) kU= nuGS(iSS,nX) -1+LU   
        if(LL.lt.0) kL= kAI1(iSS,nX) -1-LL   ! AI EL:  LL < 0
        if(LU.lt.0) kU= kAI1(iSS,nX) -1-LU  
                                             
        DE= E(kU,nX) - E(kL,nX) 
        if(DE .le. zero) then
          write(*,'(a20, i3, 2(i5,f11.3))') 'nX, kL, E, kU, E=', 
     +                                   nX, kL, E(kL,nX), kU, E(kU,nX) 	  
	    PAUSE 'Excitation down in reading Exc.inp'
        endif

        MthdEX(kL,kU)= mth
        Ax(kL,kU) = Axw
        Bx(kL,kU) = Bxw
        Cx(kL,kU) = Cxw
        Dx(kL,kU) = Dxw
        Ex(kL,kU) = Exw
        Fx(kL,kU) = Fxw

        flu(kL,kU,nX)= -fw   ! correct (positive) value of Absorption Oscillator Strength

        A(kU,kL,nX)= 4.339192d7*flu(kL,kU,nX)*DE**2 *g0(kL,nX)/g0(kU,nX)

        if(CountExc .GE. StrExc) goto 7  ! The last line of "Exc.inp"

        if(mth.eq. 5 .or. mth.eq.11 .or. mth.eq.16) goto 6   ! Present DaBa does not use other methods    

        write(*,'(a65,5i5)')'Excit CrosSec fit method is unknown for XE, 
	+  iSS, LL, LU, mth=', nX, iSS, LL, LU, mth
        PAUSE
  7     close(nFi)
      enddo        ! here XE-loop is 1 to 1, i.e. X only; C, He, D have no excited levels

c                       Read 'Params1.inp'.
      do nX= 1, nXE  
        read(13,*) iniQS   ! level number for loading initial POPs of "nX"  in zone number La  
        do La= 1, LaMx
          do k= 1, NST(nX)
            if(k.ne.iniQS) POPf(k,nX,La)= 1.d-10
            if(k.eq.iniQS) POPf(k,nX,La)= one - (NST(nX)-1)*1.d-10
          enddo
        enddo
      enddo 

      do nX= 1, nXE
        read(13,*) AtMass(nX)  ! ion mass [a.u.] for Doppler and Stark calculation of line widths
      enddo
      read(13,*) empty

      read(13,*) fluMin  ! minimal absorption oscillator strength. Spectral lines with flu < "fluMin" are 	
      read(13,*) comme   ! too weak vs continuum and/or instrumental noise, thus we exclude these lines from computations. 

      read(13,*) AulMin  ! minimal Einstein A coef. Spectral lines with A < Aul are excluded due to same reason 
      read(13,*) hvMin   ! [eV]; soft edge of hv(i) array
      read(13,*) hvMax   ! [eV]; hard edge of hv(i) array
      read(13,*) domNu   ! number of domains in array of photon energy points, hvV(iv) 
      do i = 1,  domNu
        read(13,*) hehv(i), dvKv(i)   ! hard edges of domain, relative interval (dv/v) between hv points in the domain
      enddo
      read(13,*) empty

      read(13,*) NCL    ! [== NT in manual] is the number of coaxial cylindrical tubes (see Fig.1)== 
      read(13,*) comme  ! ==  number of concentric radial belts on seen-to-detector plane round image of target, Fig.2.
      read(13,*) Ntet   ! >= 18 is number of sectors over polar angle 
      read(13,*) Nr     ! number of spheric layers in eavh zone   
      read(13,*) empty

      read(13,*) hvPrint1,     hvPrint2     ! [eV] edges of PrintOut interval of "Frames.dat".  Print (181.  
      read(13,*) hvPrint3(1),  hvPrint4(1)  ! [eV] edges of PrintOut interval of "SpIn1.dat".   Print (182 
      read(13,*) hvPrint3(2),  hvPrint4(2)  ! [eV] edges of PrintOut interval of "SpIn2.dat".   Print (183 
      read(13,*) hvPrint3(3),  hvPrint4(3)  ! [eV] edges of PrintOut interval of "SpIn3.dat".   Print (184 
      read(13,*) empty

      read(13,*) nGaLor  ! serial number of frame to be convolved with Gaussian and Lorentzian instrumental functions
      read(13,*) comme
      read(13,*) minGaLor, maxGaLor  ! [eV] Edges of interval for which instrumental function is known.. 
      read(13,*) comme               ! see comments in "Params1.inp"
      read(13,*) comme
      read(13,*) Ains, Bins, Cins    ! constants of quadratic fit to FWHM/hv of instrumental Gaussian (or 
c                                      Lorentzian), namely, FWHM/hv = Ains + Bins*(hv[keV]) + Cins*(hv[keV])^2
      close(13)

c  Read coefficients of ionization cross-section from "Inz.inp" and 
c       assign "1" to "bra(i,f,XE)" if Yes ioniz channel in "Inz.inp".

      read(12,*) StrInz  ! From "Params0" read the number of strings in file "Inz.inp" (including the title)

      do nX= 1, 1             ! nXE; X (=Kr) only
        nFi= 13+ 100*nX       ! "Inz.inp" fail # 
        read(nFi,'(a9)') title
	  CountInz = 1
  9     read(nFi,*) iSS1, iQS1, iSS2, iQS2, Axw, Bxw, Cxw, 
     +              Dxw, MePh, Exw, Fxw, Gxw, Hxw, thre    ! ioniz threshold
	  CountInz = CountInz + 1

        if(iQS1.gt.0) ki= nuGS(iSS1,nX)-1 +iQS1    ! "ki" is order# of "iQS1" in "long" numbering. Here non-AI EL    
        if(iQS1.lt.0) ki= kAI1(iSS1,nX)-1 -iQS1    ! "iQS1" is AI QS, thus its iQS1 < 0  
        if(iQS2.gt.0) kf= nuGS(iSS2,nX)-1 +iQS2   
        if(iQS2.lt.0) kf= kAI1(iSS2,nX)-1 -iQS2    ! AI EL, "iQS2 < 0)  , ............ 

        if(kiSS(kf,nX) .ne. kiSS(ki,nX)+1) then
	     write(*,*) 'XE, ki, kf =', nX, ki, kf 
           PAUSE 'Error in "InzXE...inp" level numbers'
        endif

        if(mePh.ne.4) then
           write(*,'(a30, 4i6)') 'XE, ki, kf, mePh =', nX, ki, kf, mePh	! print error info
           PAUSE 'in "Inz.inp" mePh ne 4'  
        endif

        Aix(ki,kf)= Axw    ! Aix-Dix are 4 coefs of e-impact ioniz cross-section (formula #4), see "InzXE....inp"
        Bix(ki,kf)= Bxw  
        Cix(ki,kf)= Cxw 
        Dix(ki,kf)= Dxw
        Eix(ki,kf)= Exw    ! Eix-Hix are 4 coefs of photo-ioniz cross-section, see "InzXE....inp"
        Fix(ki,kf)= Fxw 
        Gix(ki,kf)= Gxw 
        Hix(ki,kf)= Hxw 
        Eth(ki,kf)= thre   ! State-to-state non-reduced Ionization Threshold; in the compu it will be reduced by DPI(iSS1,nX)] 	           
        bra(ki,kf,nX)= one ! "one" means yes ionization channel (e-impact and phot) from "ki" to "kf". When using FAC-bases: "bra" is either 1 or 0

        if(CountInz .LT. StrInz) goto 9  
        close(nFi)
      enddo  ! nX-loop;, here Kr only

c  Read AI Probabilities from 'AIw.inp'. 
      read(12,*) StrAIw  ! From "Params0" read the number of strings in file "AIw.inp" (including the title)
      close(12)

      do nX= 1, 1              !  nXE      here only X
        nFi= 14+ 100*nX        ! "AIw.inp"  files are ## == 114, 214, 314, 414 
        read(nFi,'(a9)') title
	  CountAIw = 1
  8     read(nFi,*) iSS1, iQS1, iSS2, iQS2, AIw, trEn	  ! AI EL to EL of next SS 
        CountAIw = CountAIw + 1
c       write(*,'(4i5, e14.6, i6)') iSS1,iQS1, iSS2,iQS2, AIw, CountAIw   

        if(iQS1.gt.0) PAUSE 'non-AI initial EL in "AIw.inp"'
        if(abs(iQS1).gt.nuAS(iSS1,nX)) PAUSE 'AI EL # > AIQSs in SpS'  
        if(iQS2.gt.nuQS(iSS2,nX)) then
          write(*,'(/a30, 4i5)') 'SSi, QS1, SSf, QSf =',
     +                           iSS1, iQS1, iSS2, iQS2
          PAUSE 'In "AIw.inp" afterAI EL has # > available in SpS' 
        endif     
        if(iQS2.LT.0  .and. abs(iQS2).gt.nuAS(iSS2,nX)) then
          write(*,'(/a30, 4i5)') 'SSi, QS1, SSf, QSf =',
     +                           iSS1, iQS1, iSS2, iQS2
          PAUSE 'In "AIw.inp" AI into AI EL # > available in SpS'
        endif
	     
        ki= kAI1(iSS1,nX) -1-iQS1  ! "ki" is the  # of "iQS1" in "long" ("full") list. Remember that "iQS1" < 0:  
           
        if(iQS2.gt.0) kf= nuGS(iSS2,nX)-1 +iQS2  ! iQS2 > 0 means that final EL is non-AI; "kf" is its # in total list of EL 
        if(iQS2.lt.0) kf= kAI1(iSS2,nX)-1 -iQS2  ! iQS2 < 0 means that final EL is AI; "kf" is its # in total list of ELs 

Consistancy control 1: 
        if(kiSS(kf,nX) .ne. kiSS(ki,nX)+1) then
          write(*,'(/a50)') 'In "AIwXE.inp" SS(kf) ne SS(ki)+1:' 
          write(*,'(/   4i5,   f12.3)') nX, ki, iSS1, iQS1, E(ki,nX)   
          write(*,'( i10, 2i5, f12.3)')     kf, iSS2, iQS2, E(kf,nX)  
          PAUSE 'My STOP in consistency control, reading AIw.inp' 
        endif

        EAI(ki,kf)= trEn  
        WAiz(ki,kf,nX)= AIw 

        if(CountAIw .LT. StrAIw) goto 8  
        close(nFi)
      enddo

      read(14,*) FrL    ! [s] duration of each frame 
      read(14,*) tstep  ! [s] time step of the computation. 
      if(tstep .GT. FrL/10.) tstep = FrL/10.
         tstep = tstep *(1.-1.e-7) 
      read(14,'(a9)') comme  ! This is needed for self-consistent exit of level populations and internal radiation from initial state,

      read(14,*) strt        ! [s] start time of the run must be 30 (or more) "tstep" from center of 1st frame..
c                              see "STOP" 8 lines below
      read(14,*) StopTime  
      read(14,*) tiInf       ! time to print "...LineInfo.dat", "...EmiAbso.dat", "EffSpIns.dat"  
      read(14,*) W2max       ! maximal allowed probability of crossing 2 BSs by 1 LOS  
      read(14,'(a9)') empty  

      read(14,*)   FrP   ! time [s] of centers of 8 frames of spectral POWER from the target    [W/eV/sr]  
      if(FrP(1) .LT. strt+ 20.*tstep)                                      			                     
     +               STOP 'Take "strt" in 20 (or more) tstep fromFrP(1)'     
 
      read(14,*) tPo        ! 10 t-points of scenario
      read(14,'(a9)') comme  

      read(14,*) nuBSst  ! number of BSs in the core ("ntp" t-points)
      read(14,'(a9)') comme  
      read(14,'(a9)') comme  

      read(14,*) dc ! [mcm] distance from center of test BS to surface of core, see Fig.3. 
c                                    Note: must be R1 < dc < R2-R1
      dc= dc/1.d4   ! [um] to [cm]   
      read(14,'(a9)') empty  

      read(14,*) R1t  ! radius [um] of BSs in "ntp" t-points
      read(14,*) R2t  ! radius [um] of core in "ntp" t-points
      read(14,*) R3t  ! radius [um] of Capsule in "ntp" t-points
      read(14,'(a9)') comme  
      read(14,'(a9)') comme  

      do j = 1, ntp
         if(R3t(j) .LE. R2t(j)/(1.-2./NCL)) then
            write(*,'(/a16, i2, a32)')  'At t-point #', j, 
     +                   'R3 <= R2/(1-2/NCL); increase R3' 
            STOP 'My STOP'
         endif
      enddo

      read(14,*) rdi     ! distances [mcm] between r=0 and "diLOSn" "diagnostic" LOSs (here "diLOSn" =3)
      read(14,'(a9)') empty  

      do k = 1, ntp 
         if(nuBSst(k).lt.1) PAUSE 'STOP. Code assumes at least 1 BS..'
         R1t(k)= R1t(k)/1.d4    ! [um] to [cm]   
         R2t(k)= R2t(k)/1.d4    ! [um] to [cm]   
         R3t(k)= R3t(k)/1.d4    ! [um] to [cm]   	   
      enddo

      do k = 1, diLOSn        ! all diagnostic" LOSs 
         rdi(k)= rdi(k)/1.e4  ! [um] to [cm]   
      enddo

      do j = 1, LaMx 
         do k = 1, nXE 
            read(14,*) W1D    !  1D w-array for reading strings of Params2.inp
            do i = 1, ntp 
               nit(i,j,k) = W1D(i)  !  ion number density of each XE [i/cc] at t in La
            enddo
         enddo
         read(14,'(a9)') empty
      enddo

      do j = 1, LaMx 
         read(14,*) W1D      
         do i = 1, ntp 
            Tet(i,j) = W1D(i)  !  t-points of Te [eV] in zone #j
         enddo
      enddo
      read(14,'(a9)') empty

      do j = 1, LaMx 
         read(14,*) W1D       
         do i = 1, ntp 
           u3Dt(i,j) = W1D(i)  !  t-points of Te [eV] in zone #j
         enddo
      enddo
      read(14,'(a9)') empty

      do j = 1, LaMx 
         read(14,*) W1D       
         do i = 1, ntp 
           bpt(i,j) = W1D(i)  !  t-points of e-beam part of EED in zone #j
         enddo
      enddo
      read(14,'(a9)') empty

      do j = 1, LaMx 
         read(14,*) W1D       
         do i = 1, ntp 
           bct(i,j) = 1000.*W1D(i)  !  t-points of central energy [eV] of e-beam in EED, zone #j; [keV] to [eV]
         enddo
      enddo
      read(14,'(a9)') empty

      do j = 1, LaMx 
         read(14,*) W1D       
         do i = 1, ntp 
           bwt(i,j) = 1000.*W1D(i)  !  t-points of width [eV] of e-beam in EED for zone #j;  [keV] to [eV]
         enddo
      enddo
      read(14,'(a9)') empty

      read(14,*) nqInf  ! For how many QSs of Xx (<101) you want info regarding its contacts with other QSs?  File "Contacts.dat"
      read(14,*) LaInf  ! For what cell# ?
	do k= 1, nqInf
         read(14,*) AskInf(k) ! Serial numbers of Kr QSs chosen for printing PM-info in file (49 
      enddo 
      close(14)

c  Find "LastFr" that is number of frames before StopTime
      do k = 1, mSpe        
         if((FrP(k)+FrL/2.) .LT. StopTime) LastFr= k 
      enddo 
c     write(*,'(/a20, i3)') 'LastFr =', LastFr
      SpeY = 0.
      Return
      END	     ! of INTRO subr



      integer function char2int(symbol)
	implicit none
      character*1 symbol
        SELECT CASE (symbol)
         CASE (' ')
           char2int = 1
         CASE ('0':'9')
           char2int = ichar(symbol) - 48
         CASE ('s')
           char2int = 0
         CASE ('p')
           char2int = 1
	   CASE ('d')
	     char2int = 2
	   CASE ('f')
           char2int = 3
         CASE ('g')
           char2int = 4
         CASE ('h')
           char2int = 5
         CASE ('n')
           char2int = 0
         CASE ('l')	  ! in 4l AI ELs of [He], say neon
           char2int = 0
         CASE DEFAULT
           WRITE (*,*) symbol, ' not convertible into integer'
           PAUSE 'STOPped in function char2int'
        END SELECT
      END



      SUBROUTINE LineList()   ! List spectral lines along hv;  file #30.
      use mo1co2nov
      implicit none
      integer lnew, lmin, nXmin 
      real(8) hvCmin,	Alamda, DEul 
      open (30, file= 'LineList.dat')
      write(30,'( /a42, e7.1, a8, f6.4, a6, f4.3, a2, f5.1, a5)')   
     +    'Spectral lines with Aul >', AulMin,   ', flu >', fluMin, 
     +         'at (',  hvMin/1.d3, '-', hvMax/1.d3, ') keV'   
      write(30,'(/a112/)')     'ChE SpS   hvC(eV)    Lambda(A)     A(Hz)
     +          Upper Level                     Lower Level                  
     +   flu'

      linM4= 0
      do nX= 1, 1  ! nXE, Xx only  ! count spectral lines between all ELs of each chem. element 
        lin= 0                     ! # of Spectral Line found in [hvmin*1.1, 0.9*hvmax]
        do k  = 1, NST(nX)  ! Lower state 
        do k2 = 2, NST(nX)  ! Upper state

          if(k .eq. k2)                 goto 1 
          if(kiSS(k,nX).ne.KiSS(k2,nX)) goto 1 ! Line not possible

          lin= lin+1    
          nLoh(lin,nX)= k      ! Lower level# prior to regulation of the line numbers according to their hvC
          nUph(lin,nX)= k2     ! Upper
          DEul= E(k2,nX)-E(k,nX) 
          hvCh(lin,nX)= DEul   ! Spectral line center [eV]

          if(hvCh(lin,nX) .GE. hvMax) STOP 'Line center > hvMax' 

c         Supress weak lines
          if(flu(k, k2,nX) .LT. fluMin .or.  ! if  f < fluMin
     +         A(k2,k ,nX) .LT. AulMin .or . !     A < AulMin
     +        hvCh(lin,nX) .LT. hvMin) then  ! too-soft spectral line isn't seen above free-free continuum  .
               A(k2,k,nX) = zero             ! kill this too-weak or out-of-interval line for reducing the computation time ..
                      lin = lin-1   ! exclude this (k,k2) line from the numbering
          endif    
  1       continue
        enddo                   ! k2
        enddo                   ! k
        linM(nX)= lin           ! number of this-XE lines in the domain
        linM4= linM4 +linM(nX)  ! same for all XEs
      enddo                     ! nX 

      if(linM4.ge.MNLe) then
        write(*,'(a20,i6, a4,i6)') 'Increase MNLe=', MNLe, 'to', linM4 
        PAUSE
      endif																															    

c  Regulation according to "hvC" (thru all XE)
      do lnew = 1, linM4   ! It will be a Line# after regulation along hv (in the common line list for all XEs)
        hvCmin= 1.d7       ! arbitrary value before search
        do nX = 1, 1       ! nXE, Xx only
          do lw= 1, linM(nX)                  ! check all lines found in the range
            if(hvCh(lw,nX) .LT. hvCmin) then  ! for finding minimal hvC among all lines of all XEs
              hvCmin= hvCh(lw,nX)             ! smaller hvC found
              lmin= lw              ! remember line# of this smaller hvC
              nXmin= nX             ! remember XE#   of this smaller hvC  
            endif
          enddo
        enddo                       ! search completed
        hvC(lnew)= hvCh(lmin,nXmin)
        nLo(lnew)= nLoh(lmin,nXmin)
        nUp(lnew)= nUph(lmin,nXmin)
        nX3(lnew)= nXmin            ! XE# of this line 
        hvCh(lmin,nXmin)= 7.d7      ! excludes a possibility to pick up this line again
      enddo

c  Print common (all-XE) line list into file #30 
      do iw = 1, linM4                  
        Alamda= eVA/hvC(iw)          ! A
        write(30,'(i2, i4, f11.3, f13.4, e11.3, i6, a1, a24, i7,
     +              a1, a24, f8.3)') nX3(iw), kiSS(nUp(iw),nX3(iw)),  
     +       hvC(iw), Alamda, A(nUp(iw),nLo(iw),nX3(iw)), nUp(iw),'=', 
     +       QSname(nUp(iw),nX3(iw)),nLo(iw), '=',  
     +       QSname(nLo(iw),nX3(iw)), flu(nLo(iw),nUp(iw),nX3(iw))      
      enddo
	close(30)

      write(*,'(i7, a15)') linM4, 'spectral lines'                       
      write(*,'(a22,f10.3,a4)') 'first line has hvC =',  hvC(1), 'eV.'
      write(*,'(a22,f10.3,a4)') ' last line has hvC =', hvC(linM4),'eV.'  
      Return
      END	    ! of "LineList" subr



      SUBROUTINE hvPoints() ! Build the array of hv points        
	use mo1co2nov    
	implicit none  
      real(8) dvTOv
      integer ic, icL

      hvV(1)= hvMin    
      dvTOv = dvKv(1)
      do iv = 2, nvL
         hvV(iv)= hvV(iv-1)*(1.+dvTOv)
         if(iv .eq. nvL .and. hvV(iv) .LT. hvMax) then
            write(*,'(/a14, i6, a15, f10.3, a9, f10.3)') 
     +           'Came to nvL=', nvL, 'but hvV(hvL) =', hvV(nvL), 
     +           ' < hvMax=', hvMax
            STOP 'hvV(nvL) < hvMax; incr nvL'
         endif
         do j = 1, domNu                      ! serial number of hv domain
            if(hvV(iv-1) .LT. hehv(j). and.
     +         hvV(iv)   .GE.	hehv(j) ) then
               hvV(iv) = hehv(j)  
               dvTOv = dvKv(j+1)
            endif
         enddo          	    
         if(hvV(iv) .GT. hvMax) then
             nvM = iv
	       goto 2
         endif
      enddo

  2   Write(*,'(/i20, a27)') nvM, 'points in full hv interval'  
      write(*,'(a23, f6.2, a12, f9.1)') 'hvV(1)=',   hvV(1), 
     +                                  'hvV(nvM)=', hvV(nvM)   
c      do iv = 1, nvM 
c         write(341,'(i9, f12.3)') iv, hvV(iv)
c      enddo

Control of consistency:
      do iv = 2, nvM 
         if(hvV(iv) .le. hvV(iv-1)) then
           write(*,'(a9,i7, a11, f14.7, a15, f14.7)') 'For iv =', iv, 
     +               'hvV(iv)=', hvV(iv),  '<= hvV(iv-1)=', hvV(iv-1)             
           PAUSE 'Error #1 in hv sequence'	  
         endif
      enddo

***   Re-calculate spectral response of each detector from hv array of input files to hv array of present computation 

      do j = 1, mDet                 ! each detector
        do iv= 2, nvM 	           ! serial numbers of all hv points of the code
           do ic = 1, npInpRes(j)-1  ! serial numbers of hv points of input file    

              if(hvRes(ic,j).lt.hvV(iv) .and. hvV(iv).le.hvRes(ic+1,j)) 
     +            icL= ic
           enddo  						
           Frac= (hvV(iv)-hvRes(icL,j)) / (hvRes(icL+1,j)-hvRes(icL,j))	              		   
           Resp(iv,j)= InRes(icL,j)+ Frac*(InRes(icL+1,j)-InRes(icL,j))  ! Response on hv array of the code  
        enddo
      enddo
      Return
      END	    ! of "hvPoints" subr



      SUBROUTINE EmiAbso()  ! Compu this-La emissivity "EmTot(La,hv)" [W/cc/sr/eV] and absorption coef corrected for stimulated emission"AbTot(La,hv)" [1/cm] 
      use mo1co2nov         ! at t = ti, using all params of t= ti and POPi(EL,nX,La)  
      implicit none          
      real(8)  EmiLFul(MNLe), AbsoAm(MNLe),  ! Amplitude of bound-bound (BB) emissivity and absorptivity
     +         AlfaBB(MNLe),                 ! (nU/gU)/(nL/gL)
     +         pBB(nvL), ArPBB(MNLe), dev,           
     +         FreE, EED, FWevDop, Voigt2, alfaFB,
     +         Velo, SigPhI, contrib, SiPh, SiRR, HalfLor, VoiFWHM      

c  IMPO:  This subr and Scenario are in one La-Loop of MAIN thus thermodynamic params are just for this "La"
c  IMPO:  BUT XE-loop is closed, thus here nX= nXE+1 until nX3 in 2nd "do" loop below

c  FF  EMISSIVITY and ABSORPTIVITY due to scattering of ALL free electrons on ions of ALL XE. 
      do iv= 1, nvM  
         hveV = hvV(iv)
         emisFF(iv)= 1.21d-33*DiZ2(La)*Dene(La)         ! [W/cc/sr/eV], (BlackFold 61); 
     +               * exp(-hveV/Te(La))/sqrt(Te(La))   !	MAXWELLIAN EED
         absoFF(iv)= (2.40062092d-37*DiZ2(La)*Dene(La)  ! (Black F 70), Notice 1% correction in constant
     +                  / sqrt(Te(La))/hveV**3 )        !  to fit emiFF/absoFF= 2hv/Lamb^2 in Planck.
     +                     * (one- exp(-hveV/Te(La)))   ! Corre for stimulated emission in Maxw case. 
      enddo

c  Full-shape BB EMISSIVITY and ABSORPTIVITY due to all lines;  Also Line Lorentz width "FWevLor(lw,La)" 
      do lw= 1, linM4      ! line# runs thru all XE, being ordered with hvC(iw)
        EmiLFul(lw)= zero  ! In-Full-Shape Emissivity, W/cc/sr
        AbsoAm(lw) = zero  ! Amplitude of Absorptivity (16) except for *P(hv); i.e. [eV/cm], see (18)

        if(hvC(lw) .LT. 1.1*hvMin) PAUSE 'Line with hvC < hvMin' 
        if(hvC(lw) .GT. 0.9*hvMax) PAUSE 'Line with hvC > hvMax'

        kU= nUp(lw)
        kL= nLo(lw)
        nX= nX3(lw)  ! nX3(lw) is XE# for this line 
        if(KiSS(kU,nX).ne.KiSS(kL,nX)) PAUSE 'Confusion in SS of kU, kL'

        if(kU.LT.Nnu(nX) .AND. BE(kU,nX,La).LT. 1.d-3) goto 1  ! skip line from non-AI dead EL
        if(kL.LT.Nnu(nX) .AND. BE(kL,nX,La).LT. 1.d-3) goto 1  ! skip line onto non-AI dead EL

        if(POPi(kU,nX,La) .LT. 1.d-15)                 goto 1  ! weak line

        EmiLFul(lw)= BolJ*hvC(lw)*A(kU,kL,nX)*              ! In-Full-Shape Emissivity, W/cc/sr
     +                    POPi(kU,nX,La)*Den(nX,La)/FoPi  
        AlfaBB(lw)= zero
        if(POPi(kL,nX,La).gt.1.d-15) AlfaBB(lw)=
     +    (POPi(kU,nX,La)/g0(kU,nX))/(POPi(kL,nX,La)/g0(kL,nX))

***      POPs inversion can cause AlfaBB(lw) < 0 thus causes AbsoAm(lw) < 0 BUT 
***      SpIn remains > 0 because Abso < 0 causes (Emi/Kap) < 0  and  [1 - exp(-kL)] < 0.

        AbsoAm(lw)= 1.09760936d-16*flu(kL,kU,nX)*                ! Absorptivity (16) except "*P(hv)"; i.e. [eV/cm], see (18)
     +              POPi(kL,nX,La)*Den(nX,La)*(one-AlfaBB(lw)) 
  1     continue
     
        CALL LineLorWi(lw)   ! eV; Compu "FWevLor(lw,La)" that is Lorentz FWHM of line from "kU" to "kL"

        FWevDop= FWkVcGAU(La,nX)*hvC(lw)   ! via thermal uTi & u3D together
        HalfLor= FWevLor(lw,La)/2.
        VoiFWHM= HalfLor +sqrt(HalfLor**2 + FWevDop**2)  		  

        if(ti.LT.tiInf .and. tiInf.LE.tf) then           ! at print-time "tiInf" print in "...LineInfo.dat"
           write(45+La,'(f9.3, 2i4, f12.4, i6,a1,a24, i16,a1,a24, 2f9.3,   
     +            3f9.2, e11.3, f7.3)')  hvC(lw), nX3(lw),  kiSS(kU,nX),  	
     +          eVA/hvC(lw), kU,'=',QSname(kU,nX), kL,'=',QSname(kL,nX), 	
     +                   fwTiKvC(La,nX)*hvC(lw), fwTUKvC(La,nX)*hvC(lw),                     
     +                          QMlimit(lw,La), FWevLor(lw,La), VoiFWHM,
     +                                     POPi(kU,nX,La), flu(kL,kU,nX) 
        endif
      enddo   ! lw loop

      if(ti .LT. (strt+2*tstep)) goto 2   
      absoBB= zero     ! local plasma bound-bound absorption coef corre for stimulated emission [1/cm], XE-sum, line sum
      emisBB= zero     ! local plasma BB emissivity   [W/cc/sr/eV]
      do lw= 1, linM4  ! each "hvV(iv)" may include contributions from many lines because of their long wings.
         nX = nX3(lw)    
         kU = nUp(lw)
         kL = nLo(lw)

         FWevDop= FWkVcGAU(La,nX)*hvC(lw)  ! via u3D
         Wing   = hvC(lw)*0.3              ! Line Wing hv-Length [eV]
         ArPBB(lw) = zero                  ! Area under line shape pBB(v), was =1 until wings cut 
         do iv= 2, nvM 
           pBB(iv) = 0. 
           if(hvV(iv).GT. hvC(lw)-Wing .and. 
     +        hvV(iv).LT. hvC(lw)+Wing       ) then  
                dev = abs(hvV(iv) - hvC(lw))   
                pBB(iv) = Voigt2(FWevLor(lw,La), FWevDop, hvC(lw), dev)  ! JQSRT (Sawa) Voigt. It is slower (5x ?? than Jenya's VoigtJS) but correct to 1% 
           endif                                                 
           dhv = hvV(iv)-hvV(iv-1)
           ArPBB(lw)= ArPBB(lw) + dhv*(pBB(iv)+pBB(iv-1))/2.  ! Area under line shape < 1 because of shortened wings; thus part of photons lost. 
c                                                      	    To return these photons, the cut shape is re-norm (10 lines below) to Integ[pBB(v)dv] = 1
         enddo   ! iv
         if(ArPBB(lw) .LT. 0.2) cycle  ! Lines in "continuum domains" may vave small "ArPBB(lw)" because of not-small dv/v in the hv array

         do iv= 1, nvM                             
           emisBB(iv)= emisBB(iv)+ EmiLFul(lw) *pBB(iv) ! BB emissivity [W/cc/sr/eV] DUE TO ALL lines of this XE, at hvV(iv)
     +                                       /ArPBB(lw) ! return photons from cut wings  
           absoBB(iv)= absoBB(iv)+ AbsoAm(lw)  *pBB(iv) ! [eV/cm]*[1/eV] = 1/cm  (16) DUE TO ALL LINES
     +                                       /ArPBB(lw) ! return photons from cut wings  
         enddo     
      enddo    ! lw
  2   continue

******  BF absorptivity corrected for induced inverse process and FB emissivity, 
***                     Notations: (j,k) + hv --> (j+1, kf) + e , like in function "SigPhi" to be used
      do iv= 1, nvM 
        do nX = 1, 1   ! nXE    ! In this version of the code I excluded C,He, D from FBBF because they are excluded from AtKins, thus
          absoBFx(iv,nX)= zero  ! contribution of one nX
          emisFBx(iv,nX)= zero  
          do k = 1, NST(nX)  
             if(POPi(k,nX,La) .LT. 1.d-20)  goto 7  ! Negligible contribution    
             j= KiSS(k,nX)
             do kf= 1, NST(nX)                       
               if(POPi(kf,nX,La).LT.1.d-20) goto 6
               if(bra(k,kf,nX)  .LT. 0.5  ) goto 6  ! no channel; "bra" is 0 or 1 with FAC bases 
               if(KiSS(kf,nX)   .ne. j+1  )  PAUSE 'Check j in BF abso'
               if(Eth(k,kf) .LE. 0.) then
                  write(*,'(a20,3i5,f9.2, f5.1)') 'j, k, kf, bra, Eth=', 
     +                               j, k, kf, bra(k,kf,nX), Eth(k,kf)        			 
			    STOP 'Eth < 0 while bra > 0.5'
               endif
               BEk= Eth(k,kf) - DPI(j,nX,La)        ! min hv required for (j,k)+hv --> (j+1,kf)+e photo-ionization
               if(BEk .LT. 1.)              goto 6  ! We skip channel with BEk < 1eV because no reason to expect 
c                                                     scaling of from-FAC SigPhi over hv/BEk at so small BEk   
               if(hvV(iv) .LT. BEk+1.d-6)   goto 6  ! hv cannot Photo-Ionize k  --> kf
               FreE = hvV(iv)- BEk                               ! energy of freed electron 
               Ww= 1.46796094d-22*Dene(La)*EED(FreE)/sqrt(FreE)  ! We write "1.468" instead of "2.936../two" because 
c                                                                  proved the error in (49), see comment in BlackFold
               alfaFB = Ww * (POPi(kf,nX,La)/g0(kf,nX))
     +                     / (POPi(k ,nX,La)/g0(k ,nX))

               SiPh = SigPhI(hvV(iv))          
               contrib = Den(nX,La)*POPi(k,nX,La)* SiPh *(one - alfaFB) ! (49); 
               absoBFx(iv,nX)= absoBFx(iv,nX) + contrib               ! (50), this XE

c  FB EMISSION due to RR onto ions of current nX:  (SS=j+1,kf) + e  --> (j,k) + hv   
		  	 
               Velo= 5.93096887d7*sqrt(FreE)               ! Velocity of free electron = sq(2k*E/m); cm/s  
               SiRR= SiPh *g0(k ,nX)* hvV(iv)**2 /        
     +                    (g0(kf,nX)* FreE* 1.02199815d6)  ! (39); 1.022 MeV== 2mc^2, see mo1co2nov.for 9 digits 

               contrib= Dene(La)*Den(nX,La)*POPi(kf,nX,La)*       ! (34), (28): one kf-->k channel to (37) sum
     +                  BolJ*hvV(iv)* Velo*EED(FreE) *SiRR/FoPi   ! SigRR is expressed via "SigPhi"
 								   
               emisFBx(iv,nX)= emisFBx(iv,nX) + contrib    ! k,kf Sum (37) for this XE, [W/cc/sr/eV] 
  6          continue
             enddo    ! kf
  7        continue  
           enddo      ! k  			   
        enddo         ! nX  
      enddo           ! iv 

      absoBF= zero   
      emisFB= zero 
	  
      do nX = 1, 1    ! nXE ! Excluded C,D,He from FBBF
         do iv= 1, nvM                   
            emisFB(iv) = emisFB(iv) + emisFBx(iv,nX)   ! W/cc/sr/eV;  
            absoBF(iv) = absoBF(iv) + absoBFx(iv,nX)   ! 1/cm
         enddo  ! iv    
      enddo     ! nX

      do iv= 1, nvM  	
         emTot(La,iv)= emisBB(iv)+ emisFF(iv)+ emisFB(iv)  ! W/cc/sr/eV;                                     
         abTot(La,iv)= absoBB(iv)+ absoFF(iv)+ absoBF(iv)  ! 1/cm 
      enddo 

      if(ti.LT.tiInf .and. tiInf.LE.tf) then  ! after processing all XE, at print-info time "tiInf" print "...emiAbso.dat"
	  write(400+La,'(a98)') 
     +  'hvKeV    emisFB     emisBB     emisFF     emiTot     absoBF
     + absoBB     absoFF    absoTot'    	
        do iv= 1, nvM  
           write(400+La,'(f11.4, 19e11.4)')  hvV(iv)/1.d3,    
     +        emisFB(iv), emisBB(iv), emisFF(iv), emTot(La,iv),   
     +        absoBF(iv), absoBB(iv), absoFF(iv), abTot(La,iv) 
        enddo 
      endif
      Return
      END     ! of 'EmiAbso' subr	 




      SUBROUTINE EffSpIn() ! Mean (i.e. space- direction- average) specific intensity of radiation in zone La=1,2,3 
      use mo1co2nov        ! is "SpInEf(La,hv)" [W/eV/sr/cm2], see (II.9), (AI.0). In manual "La" is "b".
      implicit none
      real(8) SpInt(3,Ntet), AngSum(3,Nr), LaySum(3), dr1, dr2, dr3,                   
     +                       ab1, ab2, ab3, dtet, teta, rm, g3, h3, ca,   
     +                      sq1, sq2, sq3, pa1, pa2, pa3, pa4, pa5, pa6, 
     +                      Vco, Vbs, Vcap
      dtet = pin/Ntet 
      dr1  = R1/Nr  	      
	dr2  = R2/Nr  
      dr3  = (R3-R2)/Nr  
      Vbs  = (4./3.)*pin*R1**3            ! [cm3] volume of 1 BS
      Vco	 = (4./3.)*pin*R2**3            ! [cm3] volume of core
      Vcap = (4./3.)*pin*(R3**3-R2**3)    ! [cm3] volume of capsule
      pa4  = (4./3.)*R1                   ! [cm]  mean path of ray thru sphere = length of mean chord.  
c                                                 Straitforward averaging/integration gives 4R/3 [see page on the desk].
c                                                 Also Google writes "The mean chord length in a sphere is 4R/3".
c                                                 Also ratio of volume to 2D image = (4/3)piR^3/piR^2 =4R/3,
      do iv= 1, nvM																
        ab1 = abTot(1,iv)   ! absorption coef (corrected for induced emission) [1/cm] in BS	 g 
        So1 = zero                             
        if(abs(ab1) .gt. 1.d-20) So1 = emTot(1,iv)/ab1   ! [W/eV/sr/cm2]. Excluded /0; notice that "ab1" can be negative (POPs inversion)
c                                                          but "SpIn" remains > 0 because ab1 < 0 makes [1-dexp] < 0 	 
        ab2 = abTot(2,iv)   ! core around BS
        So2 = zero                             
        if(abs(ab2) .gt. 1.d-20) So2 = emTot(2,iv)/ab2   ! [W/eV/sr/cm2]. Excluded /0; notice that ... see 5 lines above

        ab3 = abTot(3,iv)   ! Capsule   
        So3 = zero                             
        if(abs(ab3) .gt. 1.d-20) So3 = emTot(3,iv)/ab3

*** in BS ****  "dc" is distance from center of test BS to surface of Core, see Fig 3. "dc" is loaded from Params2.dat
*               Uniform BS is treated as a sequence of Nr spheric layers of thickness dr1= R1/Nr and
*               outer radius = dr1*j (vs center of BS). j=1,2...Nr is serial number of layer. 

        LaySum(1)= 0. ! sum over Nr layers in BS (for present "iv")  
        do j = 1, Nr       ! spheric layers in BS 
          rm = dr1*(j-0.5) ! [cm] in BS: radius of midddle spheric surface of j-th spheric layer

          AngSum(1,j) = 0. ! sum over polar angle teta for present iv, BS, layer #j. 
c                          write(*,'(/a36, 5e9.2)') 'AngSum(1,j)=', AngSum(1,j)    !@#$%
		 
          do i = 1, Ntet        ! All sectors towards C/1,j
            teta = dtet*(i-0.5) ! is polar angle of spheric coordinates originating from C/1,j;
c                                 teta is angle vs straight line that connects C/1,,j with centers of target
            g3 = rm*Sin(teta)      ! catet (denoted "g" in Fig.3);  
            sq1= sqrt(R1**2-g3**2)		
            pa1= sq1+ rm*Cos(teta) ! ray path in BS from surface to rm. Checks: 
c                                    If teta=0 then pa1=R1+rm. If teta= pi/2 then pa1=sqrt(R1^2-rm^2). If teta=pi then pa1=R1-rm 
            h3 = R2-dc +rm  ! [cm] dist from C/1,j to center of core (denoted "h" in Fig.3)

c                     write(*,'(/a36, i7, 2i3, 9f7.2)') 
c     +	                  'iv, j, i, teta, ca, sq1, pa1, h3 =',	iv, j, i,
c     +                          teta, ca*1.e4, sq1*1.e4, pa1*1.e4, h3*1.e4     !@#$%    

            ca = h3*Sin(teta)      ! denoted "w" in Fig.3.       
            sq2= sqrt(R2**2-ca**2)	
            pa2= sq2+ h3*Cos(teta)- pa1  ! in-core path of towards-C/1,j ray. Checks: 
c                                          if teta=0 then pa2= R2+h3-(R1+rm)= 2R2-dc-R1; 
c                                          If teta=pi/2 then pa2= sq(R2^2-h3^2)-sqrt(R1^2-rm^2). 
c                                          If teta=pi then pa2= R2-h3 -(R1-rm) = dc-rm-R1+rm = dc-R1 	
            sq3= sqrt(R3**2-ca**2)
            pa3= sq3 - sq2          ! [cm] path of towards-C/1,j ray through capsule. Checks: If teta= 0 or pi, then pa3=R3-R2; 
c		                                                                                    If teta=pi/2 then ca==h3. pa3= sqrt(R3^2-h3^2)- sqrt(R2^2-h3^2) :  
            wBS=            ! probability that the ray crosses a BS along pa2. 
     +		(nuBSs-1) *           ! number of BSs affecting test BS
     +        (pa2*pin*(2.*R1)**2)  ! Note: pa2 crosses BS only if this BS has center closer than R1 to the ray,    
     +         /Vco                 ! i.e. entire BS is in volume Vray= pa2*pi*(2*R1)^2. 
c                                   If (N-1) BSs are randomly distributed in Core of volume Vco then 
c                                   probability that BS is inside Vray is (nuBSs-1) *pa2*pi*(2*R1)^2 /Vco

c            write(*,'(a36,5f8.2, e9.2)')'ca, sq2, pa2, sq3, pa3, wBS =', 
c     +		      ca*1.e4, sq2*1.e4, pa2*1.e4, sq3*1.e4, pa3*1.e4, wBS   !@#$%

***   Spectral Intensity of (Capsule+Core+BSs) radiation [W/eV/sr/cm2] delivered by ray #i to any point C/1,j belonging 
***                                                              to spherical layer of middle radius rm= dr1*(j-0.5), see (AI.1) 
            SpInt(1,i)=(So3*(1.-exp(-ab3*pa3))*exp(-ab2*pa2)  +  ! [W/eV/sr/cm2] from capsule towards rm, attanuated in core UNTIL surf of BS
     +                  So2*(1.-exp(-ab2*pa2))                +  ! radiation of core towards rm 
     +              wBS*So1*(1.-exp(-ab1*pa4))*exp(-ab2*pa2/2.)) ! radiation of one of (nuBS-1) BSs, attenuated in core on 
c                                                                mean path from surf of crossed BS to surf of test BS.  Crossed BS can be 
c                                                                at any dist from test BS along pa2, therefore I estimate this mean path as pa2/2.
     +                                        *exp(-ab1*pa1) ! attenuation of the above 3 terms in test BS on the ray path from surf of test BS to C/1,j
     +                + So1*(1.-exp(-ab1*pa1))               ! radiation of test BS itself due to the ray path from surface of test BS to C/1,j
	
            AngSum(1,j)= AngSum(1,j)+ SpInt(1,i)*Sin(teta)  ! [W/eV/sr/cm2] Summation over conic sectors (with common top at C/b,j) for 
c                                                                           spheric layer #j, see 2nd sum in (AI.0)
c                                               Note: sin(teta) does not change units. Units are in d(teta)== pi/Nttet [sr] 
c                                                     in factor (2.*pin**2)/(3.*Vbs*Ntet) in (AI.0). However, [sr] drops from this factor
c                                                     because d(teta) [sr] is divided by 4pi [sr]. Units of the above factor are [1/cm3] 

c                            write(*,'(a40, 5e9.2)') 
c     +                        'Sin(teta), SpInt(1,i), AngSum(1,j) =', 
c     +                            Sin(teta), SpInt(1,i), AngSum(1,j)     !@#$%    
          enddo    ! i;  end of summation over polar angle teta

          LaySum(1)= LaySum(1)+ AngSum(1,j)* (dr1**3)*(3*j**2 -3*j +1)  ! [W*cm/eV/sr] see (AI.0): summation over j; outer radius r(1,j)=dr1*j.
c                                                                         r(1,j)^3 - r(1,j-1)^3 = dr1^3* (j^3 - (j-1)^3 = dr1^3* (3j^2-3j+1)                  ! 
        enddo	     ! j;     end of sumation over spherical layers

        SpInEf(1,iv)= LaySum(1)* (2.*pin**2)/(3.*Vbs*Ntet)   ! [W*cm/eV/sr]*[cm^-3]= [W/eV/sr/cm2] see (AI.0)

c        write(*,'(/a46, 5e10.2/)') 'SpInEf(1,iv) [W/eV/sr/cm2], Vbs,	   !@#$%
c     + factor =', SpInEf(1,iv), Vbs, (2.*pin**2)/(3.*Vbs*Ntet)  

		
** CORE ****   Uniform CORE is treated as a sequence of Nr spherical layers of thickness dr2= R2/Nr and
*              outer radius r/2j= dr2*j [here j=1,2...Nr]  thus middle-of-Spheric-Layer radius rm(j)= dr2*(j-0.5)
*                           see Fig.4 and (AI.0)   
        LaySum(2)= 0.       ! sum over Nr in-Core layers for present "iv"  
        do j = 1, Nr        ! spheric layers in Core 
          rm = dr2*(j-0.5)  ! [cm] radius of midddle spheric surface of j-th spheric layer in core
          AngSum(2,j) = 0.  ! sum over angle teta for present iv, core, layer #j.
		  
          do i = 1, Ntet        ! All towards-C/2,j rays
            teta = dtet*(i-0.5) ! is polar angle of spheric coordinates originating from C/2,j (at any azimuthal angle). 
c                                 Direction teta=0 may be chosen arbitrarily
            ca = rm*Sin(teta)      ! catet
            sq2= sqrt(R2**2-ca**2)	
            pa2= sq2+ rm*Cos(teta) ! [cm] in-core path of towards-C/2,j ray. Checks: if teta=0 then pa2= R2+rm; 
c                                         If teta=pi/2 then pa2= sq(R2^2-rm^2). If teta=pi  then pa2= R2-rm 	
            sq3= sqrt(R3**2-ca**2)
            pa3= sq3 - sq2         ! [cm] path of towards-C/2,j ray through capsule. Checks: If teta= 0 or pi, then pa3= R3-R2; 
c		                                                                           If teta=pi/2 then pa3= sqrt(R3^2-rm^2)- sqrt(R2^2-rm^2) 
            wBS=         ! probability that the ray crosses any BS along pa2. 
     +		nuBSs *               ! total number of BSs
     +        (pa2*pin*(2.*R1)**2)  ! Note: pa2 crosses a BS only if this BS has center closer than R1 to the ray,    
     +         /Vco                 ! i.e. entire BS in cylindrical volume Vray= pa2*pi*(2*R1)^2. 
c                                   If N BSs are randomly distributed in Core of volume Vco then
c                                   probability to catch a BS inside Vray is nuBSs* Vray/Vco

c            write(*,'(/a47, i5, f6.2, i5, 6f8.2, e9.2)')
c     +          'j, rm, i, teta, ca, sq2, pa2, sq3, pa3, wBS =', 
c     +		   j, rm*1.e4, i, teta, ca*1.e4, sq2*1.e4, pa2*1.e4, 
c     +                           sq3*1.e4, pa3*1.e4, wBS           !@#$%

***         Spectral Intensity of radiation [W/eV/sr/cm2] delivered to point C/2,j by ray #i. 

*                                                                See Fig.4. and (AI.3)
            SpInt(2,i)= So3*(1.-exp(-ab3*pa3))*exp(-ab2*pa2)  +  ! [W/eV/sr/cm2] from capsule towards C/2,j, attanuated in core on 
c                                                                  the ray path from cap/core interface  to C/2,j
     +                  So2*(1.-exp(-ab2*pa2))                +  ! radiation of core on the ray path fron cap/core interface  to C/2,j 
     +              wBS*So1*(1.-exp(-ab1*pa4))*exp(-ab2*pa2/2.)  ! radiation of in-Vray BS attenuated in core on mean path from crossed BSs to C/2,j
c                                                                  Crossed BS can be at any dist from C/2,j along pa2, 
c                                                                  therefore I estimate this mean path as pa2/2.
            AngSum(2,j)= AngSum(2,j)+ SpInt(2,i)*Sin(teta)       ! [W/eV/sr/cm2] Summation over conic sectors (top at C/2,j) for spheric layer #j, see 2nd sum in (II.0)
c                                                                          Note "Sin(teta)" does not change units, [sr] come later as units of dtet =pi/Ntet
c            write(*,'(a47, 5e9.2)') 
c     +         ' Sin(teta), SpInt(2,i), AngSum(2,j) =', 
c     +           Sin(teta), SpInt(2,i), AngSum(2,j)       !@#$%   

          enddo  ! i;  end of summation over teta

		LaySum(2)= LaySum(2)+ AngSum(2,j)* (dr2**3)*(3*j**2 -3*j +1)  ! [W/eV/sr/cm2]*[cm3] see (AI.0): summation over j; 
c                                                                  		Outer radius r(2,j)=dr2*j.  r(2,j)^3 - r(2,j-1)^3 = dr2^3*(j^3 - (j-1)^3 = dr2^3*(3j^2-3j+1)                  ! 
        enddo	   ! j;  end of sumation over layers, see (AI.0)

        SpInEf(2,iv)= LaySum(2)* (2.*pin**2)/(3.*Vco*Ntet)   ! [W/eV/sr/cm2] = [W*cm/eV/sr]*[sr/sr/cm3]. Here[sr/sr] due to dtet= pi/Ntet. /cm3] due to Vco

c        write(*,'(/a46, 5e10.2/)') 'SpInEf(2,iv) [W/eV/sr/cm2], Vcore,
c     + factor =', SpInEf(2,iv), Vco, (2.*pin**2)/(3.*Vco*Ntet)         !@#$%  
	

*** CAPSULE ***  is treated as a sequence of Nr spheric layers of thickness dr3= (R3-R2)/Nr and
*                outer radius = R2+dr3*j. Here j= 1,2...Nr. 
*                Middle-of-Spheric-Layer radius rm= R2+dr3*(j-0.5) is C/3,j in Figs 5,6
*                                                               
        LaySum(3)= 0.         ! sum over Nr in-Capsule layers for present "iv"  
        do j = 1, Nr          ! spheric layers in Capsule 
          rm = R2+dr3*(j-0.5) ! [cm] j-th midddle-layer radius in Capsule; Figs 1,5,6 rm == C/3,j of Figs 5,6. 
          AngSum(3,j) = 0.    ! sum over polar angle teta for C/3,j and present iv  
          do i = 1, Ntet        ! rays fron different teta(i) towards point C/3,j
            teta = dtet*(i-0.5) ! is polar angle of spheric coordinates originating from point C/3,j that is middle of small volume dA*dr3. 
c                                 Direction teta=0 is chosen "from C/3,j to center of capsule" 
            ca = rm*Sin(teta)   ! catet;  
            if((teta.LT.pin/2.) .and. (ca.LT.R2)) then ! ray crosses core, see Fig 6
              sq3= sqrt(R3**2-ca**2) 		   
              sq2= sqrt(R2**2-ca**2)
              pa5= sq3 - sq2         ! [cm] path of towards-C/3,j ray through far-from-C/3,j side of capsule. Check: If teta= 0 then pa5= R3-R2; 
c		                                  rays at teta=pi/2 and teta=pi don't cross core, i.e. Fig. 5, not 6 
              pa2= 2.*sq2            ! [cm] in-core path of towards-C/3,j ray. Check: if teta=0 then pa2= 2R2. If teta >= pi/2 then rays don't cross core, Fig 5 
              pa6= rm*cos(teta) -sq2 ! [cm] path of towards-C/3,j ray through near-C/3,j side of capsule. Check: If teta= 0 then pa6= rm-R2; 

              wBS=           ! is the probability that ray crosses a BS along pa2. 
     +		    nuBSs *              ! number of BSs that can be crossed
     +            (pa2*pin*(2.*R1)**2) ! Note: pa2 crosses a BS only if entire BS is in volume Vray= pa2*pi*(2.*R1)^2. 
     +            /Vco                 ! N BSs are in Core of volume Vco, then probability to catch a BS inside Vray is N*Vray/Vco

c                     write(*,'(/a45, i3, i4, 10f7.2)')
c     +                  'j, i, ca, R2, dr3, rm, teta, R3, sq3, sq2 =', 
c     +                   j, i, ca*1.e4, R2*1.e4, dr3*1.e4, rm*1.e4, 
c     +		            teta, R3*1.e4, sq3*1.e4, sq2*1.e4

c                     write(*,'(a44, 6f7.2, e10.2)')
c     +                  'pa5, pa2, pa6, rm*Cos, wBS =', 
c     +                   pa5*1.e4, pa2*1.e4, pa6*1.e4, rm*Cos(teta)*1.e4, wBS

*			Follow (AI.6):
              SpInt(3,i)= (So3*(1.-exp(-ab3*pa5))*exp(-ab2*pa2)    +  ! [W/eV/sr/cm2] from capsule to core towards C/b,j; attanuated in core
     +                     So2*(1.-exp(-ab2*pa2))                  +  ! radiation of core on the ray path towards C/3,j 
     +                 wBS*So1*(1.-exp(-ab1*pa4))*exp(-ab2*pa2/2.)    ! radiation in-Vray BS attenuated in core on mean path from surface of crossed BSs to C/3,j. 
c                                                                     Crossed BS can be at any dist from C/2,j along pa2, 
c                                                                     therefore I estimate this mean path as pa2/2.   
     +                    )                      *exp(-ab3*pa6)    +  ! atttenuation of the above 3 terms in near-C/3,j side of capsule  
     +                     So3*(1.-exp(-ab3*pa6))                     ! radiation of near-C/3,j side of capsule      

            else !  Fig 5  ; Follow (AI.5)
              pa3= sqrt(R3**2-ca**2) +rm*cos(teta) ! Ray path thru capsule to C/3,j
c                                                    Check: if teta=pi/2 then pa3= sqrt(R3^2-rm^2); if teta=pi then pa3= R3-rm; teta=0 crosses core thus case Fig 6
              SpInt(3,i)= So3*(1.-exp(-ab3*pa3))   ! [W/eV/sr/cm2] from path pa3 that is thru-capsule-only

c                     write(*,'(/a52, 2i3, 9f7.2)')
c     +                  'j, i, ca, R2, dr3, rm, teta, pa3, R3, sq3, rm*Cos=', 
c     +		            j, i, ca*1.e4, R2*1.e4, dr3*1.e4, rm*1.e4, teta, 
c     +                        pa3*1.e4, R3*1.e4, sq3*1.e4, rm*Cos(teta)*1.e4
            endif         

            AngSum(3,j)= AngSum(3,j)+ SpInt(3,i)*Sin(teta) ! [W/eV/sr/cm2] Sum over conic sectors (top at C/3,j) for spheric layer #j, see 2nd sum in (AI.0)
*                                                          ! Note: Sin(teta) does not change units, see comments to "AngSum(2,j)" & "AngSum(1,j)" 
c                     write(*,'(a44, 2i4, 5e10.3)') 
c     +                      'j, i, Sin(teta), SpInt(3,i), AngSum(3,j) =', 
c     +                       j, i, Sin(teta), SpInt(3,i), AngSum(3,j)       !@#$%   
          enddo  ! i;  end of summation over teta

          LaySum(3)= LaySum(3)+ AngSum(3,j)*( (R2+dr3*j)**3 -   	 ! [W/eV/sr/cm2]*[cm3]  see (AI.0): sum over j; outer radius r(3,j)= R2 +dr3 *j.
     +		                                (R2+dr3*(j-1))**3 )         
        enddo	  ! j;  end of sumation over layers

        SpInEf(3,iv)= LaySum(3)*(2.*pin**2)/(3.*Vcap*Ntet)  ! [W*cm/eV/sr]*[sr/sr/cm3]  see (AI.0). 
c                                                              Here[sr/sr] due to dtet/4pi = (pi/Ntet)/4pi  and /cm3] due to Vco
c
c               	    write(*,'(/a46, 5e10.2/)') 'SpInEf(3,iv) [W/eV/sr/cm2], Vcap,
c                             + factor =', SpInEf(3,iv), Vcap, (2.*pin**2)/(3.*Vcap*Ntet)         !@#$% 

        if(ti.LT.tiInf .and. tiInf.LE.tf) then   ! at print-info time "tiInf" print in "EffSpIns.dat"
          if(iv.eq.1) write(61,'(a75)') 'hvKeV       BS        Core     
     + Capsule     abRbs     abRcore    abR3mR2'

           write(61,'(f10.5, 9e11.4)')   hvV(iv)/1.d3,                   ! keV 
     +        SpInEf(1,iv), SpInEf(2,iv), SpInEf(3,iv), ab1*R1, ab2*R2,  ! W/eV/sr/cm2 = kW/keV/sr/cm2 
     +                                    ab3*(R3-R2)        
        endif
      enddo   ! iv
      Return		  
      END     ! 'EffSpIn' subr




      SUBROUTINE PowYie() ! is CALLed on each t. Computes from-target Specific Power & Yield in each frame, file (181. 
      use mo1co2nov       ! This subroutine is CALLed from outside La-loop, thus on entry has La = LaMx+1 	
      implicit none  
      integer nCo   ! number of thru-core towards-target LOSs
      real(8)       SpInCC(nvL,NCL), SpInBSs(nvL,NCL), SpPowBSs(nvL), ! SumBS(nvL),    
     +	          FrGaConv(nvL), FrLorConv(nvL), SpPowCC(nvL), 
     +              ea1(nvL), ea2(nvL), ea3(nvL),  FrRe(nvL), Vo(NCL), 
     +              Vco, SumVo, drZ, hadr, ex1, ex2, ex3, SpIn1, SpIn2, 
     +                                    SpIn3,  SpinBS, taCor, taCap     ! , FBS, wBS1, rpBS  
      do iv= 1, nvM
	   ea1(iv) = zero                       ! [W/eV/sr/cm2]
         if(abs(abTot(1,iv)) .gt. 1.d-20)     ! exclude "/0." in next line;  La=1 is BS  
     +      ea1(iv)= emTot(1,iv)/abTot(1,iv)  ! in case of POPs inversion: "abTot" can be < 0  (alfa > 1) but 
c                                             "SpOut" remains > 0 because in this case [1-dexp...] is also negative
	   ea2(iv) = zero                     
         if(abs(abTot(2,iv)) .gt. 1.d-20)    
     +      ea2(iv)= emTot(2,iv)/abTot(2,iv)  

	   ea3(iv) = zero                     
         if(abs(abTot(3,iv)) .gt. 1.d-20)    
     +      ea3(iv)= emTot(3,iv)/abTot(3,iv) 

         SpPow(iv) = 0.  ! [W/eV/sr] this-t Specific power of towards-detector radiation from entire target (sum over LOSs) 
         SpPowCC(iv)= 0. ! [W/eV/sr] this-t Same except contribution of thru-Capsule-only LOSs 
      enddo
	
      Vco	= (4./3.)*pin*R2**3  ! [cm^3] volume of core
      drZ = R3/NCL             ! width of radial belt on plane round surface seen to detector 
      hadr= drZ/2.            
      SumVo = 0.                   	   
 
c      if((tiInf-tstep/2.) .LE. ti .and. ti.LT. (tiInf+tstep/2.))      !@#$%*************
c     +    write(341,'(//a32, f6.2, a7/)')  '** ti =', ti*1.e12,'ps **' 
                             
      do k = 1, NCL
         rc(k) = drZ*k - hadr             ! [cm ] center of k-th radial zone
         Ar(k) = pin*( (rc(k)+hadr)**2 
     +                -(rc(k)-hadr)**2 )  ! [cm2] area   of k-th radial zone on round plane seen by detector
         if(rc(k).le.R2) then             ! [cm]  k-th LOS crosses core
            L2= 2.*sqrt(R2**2 -rc(k)**2)  ! [cm ] thru-core path of k-th towards-detector LOS
         else	                            ! k-th LOS runs outside core, no L2 
            L2= 0.
         endif
         Vo(k) = Ar(k)*L2        ! [cm3] in-core volume of k-th cylindrical zone behind Ar(k); 
         if(rc(k).lt.R2) nCo= k  ! k-th LOS crosses Core. nCo is number of thru-Core LOSs

              SumVo = SumVo + Vo(k)                             
c              if((tiInf-tstep/2.) .LE. ti .and. ti.LT. (tiInf+tstep/2.))   !@#$%*************
c     +            write(341,'(a44, 2i4, 2f9.3, f9.2, f9.1, f7.3)')                  
c     +              'k, nCo, rc(k), L2, Ar(k), Vo(k), SumVo/Vco =',
c     +               k, nCo, rc(k)*1.e4, L2*1.e4, Ar(k)*1.e8, 
c     +                            Vo(k)*1.e12, SumVo/Vco           !@#$%****************    
      enddo ! k 

Compute SpIn(iv,k) [W/eV/sr/cm2] that is Specific intensity of from-target radiation along k-th towards-detektor LOS
        SpIn = zero
      do k = 1, nCo                 ! All THRU-CORE towards-detector LOSs
         L2= 2.*sqrt(R2**2 -rc(k)**2)        ! [cm] thru-Core path of k-th towards-detector LOS
         L3=    sqrt(R3**2 -rc(k)**2) -L2/2. ! [cm] ONE-SIDE thru-Capsule path of k-th towards-detector LOS
         do iv = 1, nvM
            ex3 = exp(-abTot(3,iv)*L3)      
            SpIn3= ea3(iv)*(1.-ex3)     ! [W/eV/sr/cm2] from far segment of Capsule into Core along k-th towards-detector LOS
            ex2 = exp(-abTot(2,iv)*L2)  ! Attenuation of from-Capsule radiation in Core on path of k-th towards-detector LOS,
c                                         neglecting possible shortening of L2 caused by possible BS along this LOS			  
	      SpIn2=                      ! [W/eV/sr/cm2] from Core to NEAR-detector Capsule along k-th LOS
     +             SpIn3*ex2            ! Attenuation in Core, neglecting possible absorption in BSs 
     +           + ea2(iv)*(1.-ex2)       ! added by Core
            SpIn(iv,k)= SpIn2*ex3 +SpIn3  ! Attenuation of FarCapsule+Core radiation in near-dete Capsule + contrib of near-dete Capsule 
         enddo   ! iv                       Note: SpIn(iv,k) is due to Core+Capsule, No BSs. 
      enddo      ! k

      do k = nCo+1, NCL               ! All outside-Core towards-detector LOSs
         L3= 2.*sqrt(R3**2-rc(k)**2)  ! path of k-th LOS through Capsule (La=3) 
         do iv = 1, nvM
            ex3= exp( -abTot(3,iv)*L3)     
            SpIn(iv,k)= ea3(iv)*(1.-ex3)  ! [W/eV/sr/cm2] on exit from target along k-th towards-detector LOS 
         enddo  ! iv                 . 
      enddo  ! k 

*** Save computed Core+Capsule SpIn(iv,k) as SpInCC(iv,k) 
Compute SpPowCC(iv) that is Power due to Core+Capsule [W/eV/sr] from target towards detector

      do k = 1, NCL       !  sum over k coaxial cylinders 
         do iv = 1, nvM
            SpInCC(iv,k)= SpIn(iv,k)                       ! SpInCC(iv,k) is due to Core+Capsule, No BSs    
            SpPowCC(iv) = SpPowCC(iv)+ SpInCC(iv,k)*Ar(k)  ! [W/eV/sr] due to Core+Capsule, No BSs yet 
         enddo  
c         write(341,'(a40, i3, 2e10.3)')                                !@#$%********************************
c     +                            'k, SpInCC(2530,k), SpInCC(3840,k)',	
c     +                             k, SpInCC(2530,k), SpInCC(3840,k)   !@#$%*******************************	
      enddo ! k

Compute "SpPowBSs(iv)" that is Contribution of BSs to SpPow(iv), 

      L1= 4.*R1/3. ! [cm] length of MEAN thru-BS chord, see long comment to word "chord"
      L2= 2.*R2/3.                       ! [cm] half of MEAN thru-CORE chord 
      L3= sqrt(R3**2 -(5./9.)*R2**2) -L2 ! [cm] one-side thru-CAPSULE path along continuation of L2

      do iv = 1, nvM
         ex1 = exp( -abTot(1,iv)*L1)     
         ex2 = exp( -abTot(2,iv)*L2)     
         ex3 = exp( -abTot(3,iv)*L3)     
         SpInBS= ea1(iv)*(1.-ex1)  ! [W/eV/sr/cm2] contribution of ONE MEAN BSs to from-target SpIn(iv,k) 

         SpPowBSs(iv)= nuBSs*SpInBS *(pin*R1**2) *ex2*ex3  ! [W/eV/sr] due to BSs
         SpPow(iv) = SpPowCC(iv) + SpPowBSs(iv)            ! [W/eV/sr] Full specific power from target (Core+Capsule+BSs)

c         if((tiInf-tstep/2.) .LE. ti .and. ti.LT. (tiInf+tstep/2.)) then     !@#$%*************
c            if(iv.eq.2530 .or. iv.eq.3840) write(341,'(a1)') '-'
c	      if(iv.eq.2530 .or. iv.eq.3840) write(341,'(a71)')  
c     +	            'iv    hvV       ea1       ea2       ea3       ex1
c     +       ex2       ex3'
c            if(iv.eq.2530 .or. iv.eq.3840) write(341,'(i5,f8.3,9e10.3)')
c     +       iv, hvV(iv)/1000., ea1(iv), ea2(iv), ea3(iv), ex1, ex2, ex3 

c            if(iv.eq.2530 .or. iv.eq.3840) write(341,'(a1)') '-'
c            if(iv.eq.2530 .or. iv.eq.3840) write(341,'(a53)') 
c     +          'iv    hvV     SpInBS    SpPowCC   SpPowBSs   SpPow'
c            if(iv.eq.2530 .or. iv.eq.3840) write(341,'(i6,f8.3,9e10.3)')
c     +           iv, hvV(iv)/1000., SpInBS, SpPowCC(iv), SpPowBSs(iv), 
c     +                                                   SpPow(iv)
c            if(iv.eq.3840) write(341,'(a1)') '-'
c         endif                                                            !@#$%*************
      enddo       ! iv 									

		   	
Collect Radiation Yield during each of "LastFr" frames [J/eV/sr] i.e. t-integrate "SpPow(iv) within a frame 
      do j = 1, LastFr    
        if((FrP(j)-FrL/2.).LE.ti .and. ti.LT.(FrP(j)+FrL/2.)) then   ! ti is inside Frame #j
          do iv = 1, nvM 
            FrYie(iv,j)= FrYie(iv,j) + tstep*SpPow(iv)  ! [J/eV/sr] From-target Radiation Yield during the frame
          enddo                          
        endif
      enddo   

Collect full-time energy radiated by the target [J/eV/sr] since t0 till StopTime  
      do iv = 1, nvM 
         SpeY(iv)=  SpeY(iv) + tstep*SpPow(iv)    ! [J/eV/sr] = [kJ/keV/sr]                       
      enddo

      if(tf .GE. StopTime) then  ! Last visit to subr "PowYie"	
        do iv = 1, nvM		
           if(hvPrint1.LE.hvV(iv) .and. hvV(iv).LE.hvPrint2)             ! hv-interval of "Frames.dat" 
     +        write(181,'(f10.6, 29e11.4)') hvV(iv)/1.e3,                ! [keV]   
     +            FrYie(iv,1), FrYie(iv,2), FrYie(iv,3), FrYie(iv,4),    ! [kJ/keV/sr] Energy radiated by the target during one frame
     +            FrYie(iv,5), FrYie(iv,6), FrYie(iv,7), FrYie(iv,8),  
     +                              SpeY(iv),                            ! [kJ/keV/sr] Energy radiated by the target since t0 till "StopTime"  
     +                              Resp(iv,1),                          ! [arb.u.] Response of 1st detector on hv array of the code. The 1st detector is spectrometer (that for frames of spectral yield)                           !                              
     +            FrYie(iv,1)*Resp(iv,1), FrYie(iv,2)*Resp(iv,1),        ! [arb.u.]
     +            FrYie(iv,3)*Resp(iv,1), FrYie(iv,4)*Resp(iv,1), 
     +            FrYie(iv,5)*Resp(iv,1), FrYie(iv,6)*Resp(iv,1),        ! [arb.u.]
     +            FrYie(iv,7)*Resp(iv,1), FrYie(iv,8)*Resp(iv,1), 
     +                              SpeY(iv)*Resp(iv,1)                  ! [arb.u.]
        enddo                                                            

Convolve one frame (# nGauLor) with Gaussian and Lorentzian instrumental functions 
        FrGaConv = zero
	  FrLorConv= zero
        do iv = 1, nvM 
           FrRe(iv)= FrYie(iv,nGaLor)*Resp(iv,1)  ! [arb.u.] 2D to 1D array 
        enddo		    
        CALL GauInstrConvo(FrRe, FrGaConv)  ! Convolution with Gaussian instrumental function 
        CALL LorInstrConvo(FrRe, FrLorConv) ! Convolution with Lorentzian instrumental function 

        do iv = 1, nvM		
          if(hvV(iv). LE. MinGaLor) cycle
          if(hvV(iv). GE. MaxGaLor) cycle
          write(191,'(f10.6, 9e11.4)') hvV(iv)/1.e3,                      ! [keV]   
     +                             FrRe(iv), FrGaConv(iv), FrLorConv(iv)  ! [arb.u.]
        enddo				   
      endif  


********* IMPO: ******** For thru-Core LOSs express "SpInBSs(iv,k)" via "SpPowBSs(iv)" 
c   Comparison of Frame 1 from "Core-only" (i.e. tiny empty BSs, thin empty Capsule) to 
c   Frame 1 from "3 BSs of same total mass" (i.e. empty Core & Capsule) showed that my computations 
c   via "wBS in SpIn(iv,k)" (see "Co2nov2025_Save Nov22.for") doesn't provide equality of those Frames,
c   while present computation of "Frame from 3 BSs" [where contribution of BSs is included in "SpPow(iv)", 
c   not in "SpIn(iv,k)" provides the equality. 
c        However,I need "SpIn(iv,k) for r-images & diagnostic LOSs. BSs don't add to Outside-Core "SpIn(iv,k)",
c   but BSs add to "SpPow(iv)". To see the contribution of BSs to Thru-Core "SpIn(iv,k)",
c   I distribute "SpPow(iv)" among thru-Core LOSs according to Vo(k)/Vco (only for t = tiInf):   
	   
      if((tiInf-tstep/2.) .LE. ti .and. ti.LT. (tiInf+tstep/2.)) then  
          do iv = 1, nvM
          do k  = 1, nCo    ! only thru-core LOSs
                SpInBSs(iv,k)= (SpPowBSs(iv)/pin/R2**2) *Vo(k)/Vco  ! [W/eV/sr/cm2]  ***********  ??????????  **********
                SpIn(iv,k) = SpInCC(iv,k) + SpInBSs(iv,k)           ! [W/eV/sr/cm2] Radiation specific intensity from target (Core+Capsule+BSs)
          enddo
          enddo 
      endif      


********  Radial images of spheric target at ti = tiInf. Each image is intergral over full hv-interval 
********  with account of spectral responce. We use rc(k) & SpIn(iv,k) computed above.  

      if((tiInf-tstep/2.) .LE. ti .and. ti. LT. (tiInf+tstep/2.)) then   
          do i = 1, imgs                 ! all imaging cameras     
            do k = 1, NCL                ! LOSs r-points in each picture
               Image(i,k) = 0.	   
               do iv = 2, nvM	
                 dhv = hvV(iv) - hvV(iv-1)			  	
                 Image(i,k)= Image(i,k) + 
     +		            SpIn(iv,k)*Resp(iv,4+i)*dhv 
               enddo   ! iv
c                           write(341,'(a46, i2, i4, 5e10.3)')                       !*********************  
c     +                       'i, k, SpIn(2530,k), SpIn(3840,k), Image(i,k) =',      
c     +                        i, k, SpIn(2530,k), SpIn(3840,k), Image(i,k)          !*********************       
               write(184+i,'(f7.2, f9.4)') rc(k)*1.e4,            ! [cm] to [mcm] 
     +                                Image(i,k)/Image(i,1)         
            enddo      ! k-th LOS  
            close(184+i)
          enddo        ! i-th camera           
      endif

********  Specific intensity of radiation on exit from the target along 1,2,3-th "diagnostic" LOSs is 
********  denoted "diSpIn(hv, diLOS#)".[W/eV/sr/cm2] & is calculated at ti = "tiInf" using known "SpIn(iv,k)" 
	
      if((tiInf-tstep/2.) .LE. ti .and. ti .LT. (tiInf+tstep/2.)) then
         do i = 1, diLOSn       ! all "diagnostic" LOSs      
         do k = 1, NCL - 1      ! r-points 
            if(rc(k).lt.rdi(i) .and. rdi(i) .le. rc(k+1)) then 
              frac = (rdi(i)-rc(k))/(rc(k+1)- rc(k))
	        do iv= 1, nvM
		       diSpIn(iv,i)= SpIn(iv,k)+ frac*(SpIn(iv,k+1)-SpIn(iv,k))  
              enddo  !  iv  
            endif
         enddo      ! k		      
         enddo      ! i

         open(182, file= 'SpInDi1.dat')  ! Specific Intensity [kW/keV/sr/cm2] along 1st "diagnostic" LOSs on its exit from target. 
         open(183, file= 'SpInDi2.dat')  ! Specific Intensity [kW/keV/sr/cm2] along 2nd ...... 
         open(184, file= 'SpInDi3.dat')  ! Specific Intensity [kW/keV/sr/cm2] along 3rd ........ 

         do i = 1, diLOSn              ! all "diagnostic" LOSs (here diLOSn =3)     
            write(181+i,'(a60)')   'hvKeV       Phys    tauCore   tauCap
     +    Respon    SpeArbU'

           L2= 2.*sqrt(R2**2 - rdi(i)**2)        ! [cm] thru-Core path of i-th diagnostic LOS
           L3=    sqrt(R3**2 - rdi(i)**2) -L2/2. ! [cm] ONE-SIDE thru-Capsule path of i-th "diagnostic" LOS
           do iv = 1, nvM		
             if(hvPrint3(i).LE.hvV(iv) .and.hvV(iv).LE.hvPrint4(i)) then   ! in hv-interval of i-th diagnostic LOS .
               taCor = abTot(2,iv)*L2
               taCap = abTot(3,iv)*L3
                write(181+i,'(f10.6, 9e10.3)') hvV(iv)/1.e3,              ! [keV]   
     +             diSpIn(iv,i),                                         ! [kW/keV/sr/cm2] Specific Intensity of radiation on exit from target along i-th "diagnostic" LOS 
     +		     taCor, taCap,
     +             Resp(iv,1+i),                                         ! [arb.u] Response of (1+i)-th spectrometer (that on i-th diagnostic LOS) .
     +             diSpIn(iv,i)*Resp(iv,1+i)                             ! [arb.u].Diagnostic LOS #i is towards spectrometer #(i+1)    .                           !                              
  		   endif ! hvV
           enddo   ! iv   
           close	(181+i)
         enddo     ! i   
      endif        ! ti  


********  Specific intensity of radiation [W/eV/sr/cm2] on three interfaces along central LOS (that through r=0) 
********  at ti = tiInf. To be printed in file "AlongCeLOS.dat". No spectrometer, no spectral response

      if((tiInf-tstep/2.) .LT. ti .and. ti .LE. (tiInf+tstep/2.)) then 
         open(333, file= 'AlongCeLOS.dat')
         write(333,'(a67)')         'hvKeV   Interf1   Interf2   Interf3
     +    k1xL1    k2x2R2   k3xR3mR2'	 

         L1= 4.*R1/3.    ! [cm] MEAN thru-BS chord, see long comment to word "chord". 
         L2= 2.*R2       ! Through-Core path of central LOS
         L3= R3-R2                                ! Central LOS path thru Capsule before entry into Core
         wBS= (nuBSs*L2*(1.5*R1)**2)/1.333/R2**3  ! probability to catch a BS by central LOS.  
c                                                 The LOS traverses a BS only if BS has center closer than R1 to the LOS,  
c                       in average, at R1/2 from the LOS, i.e. inside volume VLOS= L2*pi*(1.5*R1)^2.  
c                       N BSs are randomly distributed in Core of volume (4/3)*pi*R2^3, 
c                       then probability to find at least one BS in VLOS is  nuBSs*L2*pi*(1.5*R1)^2/[(4/3)pi*R2^3] 	
         do iv= 1, nvM
	      So1 = zero                        ! [W/eV/sr/cm2]
            if(abs(abTot(1,iv)) .gt. 1.d-20)  ! exclude "/0." in next line;  La=1 is BS  
     +         So1 = emTot(1,iv)/abTot(1,iv)  ! in case of POPs inversion: "abTot" can be < 0  (alfa > 1) but 
c                                               "SpOut" remains > 0 because in this case  [1-dexp...] is also negative
            So2 = zero
            if(abs(abTot(2,iv)) .gt. 1.d-20)  ! exclude "/0." in next line;  La=2 is Core
     +         So2 = emTot(2,iv)/abTot(2,iv)  

            So3 = zero                        ! [W/eV/sr/cm2] = [kW/keV/sr/cm2]
            if(abs(abTot(3,iv)) .gt. 1.d-20)  ! exclude "/0." in next line;  La=3 is Capsule  
     +         So3 = emTot(3,iv)/abTot(3,iv) 
	 
            ex3 = exp(-abTot(3,iv) *L3) ! remenber central LOS   
            ex2 = exp(-abTot(2,iv) *L2)  
            ex1 = exp(-abTot(1,iv) *L1)
            SpIn1 = So3*(1.-ex3)      ! [W/eV/sr/cm2] from Capsule into Core along central LOS
            SpIn2 =                   ! [W/eV/sr/cm2] from Core into Capsule along central LOS
     +              SpIn1*ex2         ! Attenuation of SpIn1 in Core, neglecting absorption in BS (due to low probability to catch a BS)  
     +            + So2*(1.-ex2)      ! added by the Core
     +            + So1*(1.-ex1)*wBS  ! added by BSs. It includes the probability of catching a BS   
            SpIn3 = SpIn2*ex3 +SpIn1  ! [W/eV/sr/cm2] Radiation intensity on exit from the target along central LOS 

            write(333,'(f7.3, 9e10.3)') hvV(iv)/1.e3, SpIn1,SpIn2,SpIn3,   ! [keV], [kW/keV/sr/cm2]  	
     +                    abTot(1,iv)*L1, abTot(2,iv)*L2, abTot(3,iv)*L3       
         enddo    ! iv
         close(333)
	endif

Compute this-ti signals of three Power Detectors (former PCDs) using each-ti "SpPow(iv)" and
******** spectral response of these detectors "Resp(iv,j)",  Instruments # = 6+j =7,8,9.

      PowDet = 0.  ! [GW]
      PDsign = 0.  ! [arb.u.]
      do iv = 2, nvM
         dhv = hvV(iv)-hvV(iv-1)
         PowDet= PowDet + SpPow(iv)*dhv  ! [W/sr]  
         do j = 1, nPD                                        ! all power detectors
	      PDsign(j)= PDsign(j)+ SpPow(iv)*dhv*Resp(iv,6+j)      ! [arb.u]
            if(iv.eq.nvM) write(186+j,'(f6.1, 2e12.4)') ti*1.e12, ! [ps] 
     +              	              PowDet*FoPi/1.e12, PDsign(j)  ! [TW], [arb.u.] 
         enddo  ! j
      enddo	  ! iv  

      Return 					  								 
      END     ! of 'PowYie subr



      SUBROUTINE AtKins()  ! For known La,nX run Atomic Kinetics from "ti" to "tf" using   
      use mo1co2nov        ! all params, W's, POPs  of t=ti. Note: these POPs are POPi(k,nX,La).  
      implicit none        ! "Atkins" produces POPf(k,nX,La), that are POPs at t = tf.      						  
      real(8)  Sum1
      external POPdot  ! subroutine mentioned by name in arguments of NAG d02
 
c      write(*,'(a40)') 'Came to CALL Ws'
      
      CALL Ws()  ! calculate all transition probabilities mentioned in the Rate Equations

c      write(*,'(a40)') 'Passed Ws'

      CALL PMg0  ! PM(k,kf), Wout(k), WoutXE(k,nX,La) for g0 case		 

c      write(*,'(a40)') 'Passed PMg0'

      do k= 1, NST(nX)
        POP(k) = POPi(k,nX,La)   ! Load POPs in 1D array. 1D is request of d02 
      enddo

********************  Integrate the rate equations:
      ifail= -1
      Scale= 1.d40
      tiS= ti*Scale
      tfS= tf*Scale

      if(nX .ne. 1) PAUSE ' Came to the rate eqs with nX=/= 1'   ! with this DaBa it must be X only; 

      CALL D02EAF(tiS, tfS, NST(nX), POP, tolD02, POPdot, WEAF, Nwork,  
     +            ifail) 
      if(ifail.ne.0) PAUSE 'ifail =/= 0 in d02. My STOP'  
	
Consistency Control of POPs obtained:
      SumP= zero 
      do k= 1, NST(nX)
	  if(BE(k,nX,La).lt.1.d-3 .AND. POP(k).gt.zero) then   
          write(*,'(a36, i4, a29)') 'After d02 found POP on dead lev#',
     +    k, ', which has BE, POP and POPi ='
          write(*,'(f9.4, 2e11.3)')  BE(k,nX,La), POP(k), POPi(k,nX,La)	  
	    PAUSE 'POP in dead EL'
        endif       

	  if(POP(k).lt.-1.d-9) then                       ! Note: NAG d02 with "tolD02"= 1.d-7 at some t-step gave |sumPOP - 1| = 1.e-8   
	     Write(*,'(//a29, e10.2, a17, i2, i4, e14.6/)')     ! Some very small POPs may be negative |POP|<1.e-12
     +       'found POP < -1.d-9;  POP=', POP(k),             ! If these negative values are < -1.e-9 we send screen info 
     +       'for XE, EL, BE=', nX, k, BE(k,nX,La)
           POP(k) = zero                                ! change them to 0     	
        endif 
        sumP= sumP+ POP(k)
      enddo

      if (abs(SumP-one) .gt. 1.d-5) then 	     
          write( *,'(/a68, 1pe9.1)')  'My STOP after d02 because SumPOPs
     + differs MUCH from 1, namely, by',  SumP-one 
          PAUSE
      endif

Correction of POPs to SUM==1
      Sum1= zero 
      do k= 1, NST(nX)
         POP(k)= POP(k)/sumP
         sum1= sum1 + POP(k)
      enddo
c        write(*,'(a35,1pe10.2)') 'Corrected: SumPOPs-1 =', Sum1-one 
      if(abs(Sum1-one).gt. 1.d-14) PAUSE 'POOR correction'

c  save 1D arrays POP(k) and BE(k) [computed for "tf"] in 3D arrays for loading in POP and BE in 
c                                 Scenario of next "ti" == present "tf" for proper "La" and "XE" 
      do k= 1, NST(nX)
        POPf(k,nX,La)= POP(k)	 ! save as 'tf" value for loading in POPs in Scenario for next ti
      enddo

      do j= FSS(nX), HSS(nX)+1
        POPZ(j,nX,La)= zero    ! relative abundance of ionization stage
      enddo
      do k= 1, NST(nX)
        POPZ(KiSS(k,nX),nX,La)= POPZ(KiSS(k,nX),nX,La) + POP(k) 
      enddo

      do j= FSS(1), HSS(1)+1                                ! all SS of nX = 1
         if(POPZ(j,1,La) .lt. 1.d-40) POPZ(j,1,La)= 1.d-40  ! FOR ORIGIN processing 
      enddo
      write(115+La,'(f7.2, f7.3, 20e10.4)') tf*1.d12, Te(La)/1000.,    
     +                         Dene(La), Den(1,La), POPZ(HSS(1)-6,1,La), 
     +                         POPZ(HSS(1)-5,1,La), POPZ(HSS(1)-4,1,La),.
     +    POPZ(HSS(1)-3,1,La), POPZ(HSS(1)-2,1,La), POPZ(HSS(1)-1,1,La), 
     +                         POPZ(HSS(1)  ,1,La), POPZ(HSS(1)+1,1,La)
      Return
      END   ! of 'AtKins' subr




      SUBROUTINE redPI()  ! For this La and nX=1 this subr gives from-the-ground-state  
      use mo1co2nov       ! ionization energy of ions (PIR) in the Ion-Sphere approach           
      implicit none                                    
      real(8) SpheR       

      do jSS = FSS(nX), HSS(nX)                       ! all ionizible SSs; jSS is spectroscopic symbol    
        SpheR= ( 3.*(jSS-1)/FoPi/Dene(La) )**third    ! [cm], jSS-1 is the charge of ion, see Manual (A.2)         
        DPI(jSS,nX,La)= 2.*RyeV *jSS *a0/SpheR        ! [eV], Manual (A.5). 
        PIR(jSS,nX,La)= PI(jSS,nX) - DPI(jSS,nX,La)         
        if(PIR(jSS,nX,La) .LT. PI(jSS,nX)/2.)	PAUSE '  PIR too low'
      enddo    

      write(540+La,'(f7.2, f7.3, 20e10.4)') ti*1.d12, Te(La)/1000.,    
     +                          Dene(La), Den(1,La), 
     +                          PIR(HSS(1)-6,1,La)/PI(HSS(1)-6,1), 
     +                          PIR(HSS(1)-5,1,La)/PI(HSS(1)-5,1),
     +                          PIR(HSS(1)-4,1,La)/PI(HSS(1)-4,1), 
     +                          PIR(HSS(1)-3,1,La)/PI(HSS(1)-3,1),
     +                          PIR(HSS(1)-2,1,La)/PI(HSS(1)-2,1),
     +                          PIR(HSS(1)-1,1,La)/PI(HSS(1)-1,1), 
     +                          PIR(HSS(1)  ,1,La)/PI(HSS(1)  ,1) 
      Return
      END     ! of 'redPI' subr




      SUBROUTINE LevWi()  ! For these La,nX compu FWHM of all energy levels "LvJW(La,nX,k)" [eV] following Refs [8,9]. 
      use mo1co2nov       ! Also compu "LvLorW(La,nX,k)" [eV] that is life-time-limited "LvJW(La,nX,k)"; 
      implicit none  

c  in this subr e = m = (h/2pi) = 1; for conversion to CGS units see http://en.wikipedia.org/wiki/Atomic_units 
c  the energy   unit is 1 hartree = 27.211396 eV = 2*Ry = 4.359810e-18 J; "har" in code
c  the length   unit is 1 bohr    = 0.52917725e-8 cm; in the code == a0
c  the velocity unit is c/137.036 = 2.187691e+08 cm/s NIST CODATA; electron velo on the 1st Bohr orbit
c                    or c*Alfa  ,   where Alfa = 1/137.036 is the fine structure constant of Sommerfeld (1916)
c  the E-lield  unit is   e/a0^2  = 5.14220652e9 V/cm 

      TeAU = Te(La)/har     ! 1 hartree = 27.211396 eV
      TiAU = Tion(La)/har          
      DeAU = Dene(La)*1.48185e-25  ! Dene [e/cc] * a0^3 [cm^3/AU of volume] = e/(AU of volume) 
      do iX= 1, nXE                       ! types of perturbing IONs, including "radiator" [which has nX of CALL "AtKins"]  
        DiAU(iX)= Den(iX,La)*1.48185e-25 
         WSr(iX)= (CoSp/DiAU(iX))**third              ! Bohr, JS (3.1) dist betw ions of same XE (Wigner-Seitz radius)   
         Deb(iX)= sqrt(TiAU/FoPi/DiAU(iX))/ZC(iX,La)  ! Bohr, JS (3.4) Debye radius via ions of only one XE  
         HoF(iX)= 2.6031*ZC(iX,La)*DiAU(iX)**0.6667   ! Holtzmark field due to ions of one XE; JS (3.49); 2.60311 = 2pi(4/15)^2/3 
      enddo                                           ! add electrons to the perturbers:

      WSr(nXE+1)= (CoSp/DeAU)**third    ! dist betw electrons; J (3.1)   
      HoF(nXE+1)= 2.6031*DeAU**0.6667   ! due-to electrons Holtzmark field (3.49)
      Deb(nXE+1)= sqrt(TeAU/FoPi/DeAU)  ! electron Debye radius; JS (3.4) 
      DeF(nXE+1)=        Deb(nXE+1)                     ! "Full" Debye radius JS (3.5) for electrons equals to "pure-e" because no lighter particles      
      DeF(4)= 1./sqrt(1./Deb(nXE+1)**2 + 1./Deb(4)**2)  ! "4" is D. Full Debye radius JS (3.5) for D is Sum over e and H

      DeF(3)= 1./sqrt(1./Deb(nXE+1)**2 + 1./Deb(4)**2   ! "3" is He. For He .... is Sum over e, D, He   
     +                                 + 1./Deb(3)**2) 

      DeF(2)= 1./sqrt(1./Deb(nXE+1)**2 + 1./Deb(4)**2   ! "2" is Carbon. For C ...  is Sum over e, D, He, C 
     +                  + 1./Deb(3)**2 + 1./Deb(2)**2) 

      DeF(1)= 1./sqrt(1./Deb(nXE+1)**2 + 1./Deb(4)**2   ! "1" is X.   For X .... is Sum over e, D, He, C, X 
     +                  + 1./Deb(3)**2 + 1./Deb(2)**2  
     +                                 + 1./Deb(1)**2) 

      do iX= 1, nXE+1              ! perturbing ions and electrons 
        wwr    = WSr(iX)/DeF(iX)         ! ratio of Wigner to Debye radii for each type of e,i
        ipp(iX)= (1.+wwr)*exp(-1.5*wwr)  ! JS-factor Eta/pp (3.51a)
      enddo
      do iX= 1, nXE                             ! perturbing ions 
        irp(iX,nX) = exp(-ZC(nX,La)*ZC(iX,La)/  ! radiator * perturber (that may be also =nX); 
     +                          WSr(iX)/TiAU )  ! JS-factor Eta/rp (3.51b) for each type of ions
      enddo
      irp(nXE+1,nX)= exp(-ZC(nX,La)*(-1. )/   ! same factor Eta/rp (3.51b) for e; 
     +                    WSr(nXE+1) /TeAU )  ! IMPO: e-charge = -1 in Jenya's formulary 

      do iX= 1, nXE                           ! perturbing ions 
         vel(iX)= sqrt(TiAU/MuAU/AtMass(iX))  ! thermal velo of perturbing ion as defined by JS (3.11)
      enddo
      vel(nXE+1)= sqrt(TeAU)   ! thermal velo of perturbing electrons as defined by JS (3.11)

      do iX= 1, nXE                                         ! perturbing ions 
        fre(iX,nX) = sqrt(vel(iX)**2 + vel(nX)**2)/WSr(iX)  ! for ELs of nX, microfield frequency due to perturbers of one type JS (3.50)
      enddo
      fre(nXE+1,nX)= sqrt(vel(nXE+1)**2 + vel(nX)**2)/WSr(nXE+1)  ! microfield frequency due to perturbers electrons JS (3.50); 
 
      do k= 1, NST(nX)            ! compute Stark FWHM of all energy levels of La,nX that CALLed "LevWi"
        if(k .eq. Nnu(nX)) cycle  ! skip nucl

        do iX= 1, nXE+1                           ! perturbing ions and electrons 
          Wqs(iX)= 4.32*(pqn(k,nX)**2)*HoF(iX)*   ! A.U., LEVEL quasistatic Stark width due to micro-field of ONE type of perturbers (3.51).  
     +             ipp(iX)*irp(iX,nX)/kiSS(k,nX)   
          if(pqn(k,nX).eq.1 .and.  orb(k,nX).eq.0) Wqs(iX)=0.  ! 1s and 1s2 configs consist of 1 level thus no Stark splitting; Level width will be Baranger' 
          if(orb(k,nX).eq.0 .and. eqEL(k,nX).eq.2) Wqs(iX)=0.  ! ns^2 outer subshell is closed thus config consists of one 1S0 level thus no Stark spliting; Level width will be Baranger' 
          if(orb(k,nX).eq.1 .and. eqEL(k,nX).eq.6) Wqs(iX)=0.  ! same for np^6 outer subshell, say Ne-like 1s2 2s2 2p6 1S0 but ELs type F-like 1s2 2s 2p6 also
        enddo                                                  ! get Wqs(iX)=0 because my code does not operate quantum numbers of inner subshells.

        do iX= 1, nXE+1                              ! perturbing ions and electrons 
          R352(iX)=  Wqs(iX)/fre(iX,nX)/irp(iX,nX)   ! ratio JS (3.52) 
          f353(iX)= R352(iX)/(R352(iX)+0.5)          ! ratio JS (3.53)
           Sta(iX)= f353(iX) * Wqs(iX)       ! LEVEL Stark width due to perturbers of one type JS (3.54)
        enddo
        totSt = zero     ! Stark width due to perturbers of all types. JS (3.55)
        do iX= 1, nXE+1                ! perturbing ions and electrons 
          totSt= totSt + Sta(iX)**1.5	 ! JS (3.55)
        enddo
        LvJW(La,nX,k)= har*totSt**0.66667  ! eV; har = 27.211396eV  converts from AU to eV. 
c                                          This is Level Stark width due to ALL perturbers (3.55)

        if(LvJW(La,nX,k) .lt. 1.d-3) then  ! negligible Stark, thus no need in its reduction
             k1 = 0                        ! EL# E-nearest to "k";  here only for printOut below	  
            nDE = 0.                       ! only print  
           Frac = 1.
           goto 3  
        endif

        k1= -1              ! preliminary value
        nDE= 77777.         ! preliminary
        do kw = 1, NST(nX)                     
          if(kw .eq. Nnu(nX) .or. kw.eq.k) cycle  ! skip nucl or self "k"
          if(KiSS(kw,nX) .ne. KiSS(k,nX) ) cycle  ! we look for k,kw of same SS  
          if( pqn(kw,nX) .ne.  pqn(k,nX) ) cycle  ! we look for k,kw of same p.q.n.  
          if(eqEL(kw,nX) .ne. eqEL(k,nX) ) cycle  ! we look for k,kw with same number of outer-shell e 
          if(abs(E(kw,nX)-E(k,nX)).lt.nDE) then   ! "kw" is E-closer to "k" then previous candidate; they may differ in orb.q.n.
                  k1= kw                          ! remember for print
                  nDE = abs(E(k1,nX)-E(k,nX))     ! better choise
          endif					                      			                   
        enddo
        if(k1 .eq. -1) then  ! didn't find a neighbour. It happens when DaBa has only one EL in some config, 
            nDE = 0.         ! say C-like AI=2p6 which is highly non-H-like term with only 1 sub-level.
           Frac = 0.         ! therefore I reduce Stark to zero.  Lor will be Baranger'
           goto 3            ! k1 = -1 will be the mark of this decision.
        endif	  

        Frac = LvJW(La,nX,k) / (LvJW(La,nX,k) +nDE)  ! Jenya 
  3     LvJW(La,nX,k)= LvJW(La,nX,k) * Frac

c       Minimal width of energy level is given by the uncertainty principle "dE*dt > h/2pi", where
c       "dt" is LifeTime of the level.  Natural (LifeTime) Lorentz FWHM on hv axis is Gamma/2pi, where Gamma  
c       has the meaning of decay rate of oscillator power [Gr3 p.6], i.e. (2.74) sum of A-coefs from "k". 
c       The LifeTime is taken in Baranger' manner, 

        LvLorW(La,nX,k)= LvJW(La,nX,k) + hBar* WoutXE(k,nX,La)  ! eV

      enddo   ! k 
      Return
      END     ! of 'LevWi' subr



      SUBROUTINE LineLorWi(li)  ! Compute Lorentz FWHM of spectral Line "FWevLor(li,La)"; to be used for Voigt line shape. 
      use mo1co2nov               
      implicit none        
      integer li, qU, qL   
      qU= nUp(li)   
      qL= nLo(li)
      if( nX3(li) .ne. nX)  PAUSE  'Why CALLed from line of other XE ?' 
      if(KiSS(qU,nX) .ne. KiSS(qL,nX)) PAUSE 'Line betw ELs of dif SpSs'	 

      FWevLor(li,La)= abs(LvJW(La,nX,qU) - LvJW(La,nX,qL))                ! eV;  Reduced-Stark part of Lor FWHM of future Voigt FWHM
      if(pqn(qU,nX) .eq. pqn(qL,nX)+1) FWevLor(li,La)= FWevLor(li,La)/2.  ! Jenya's empiric (?) correction (3.51) for close pqn
c                Note: "LvJW(La,nX,qU)" and "LvJW(La,nX,qL)" are J-reduced, therefore for Lines 
c                type "from 2p2 to 1s2p" the line width "FWevLor(li,La)" became non-Zero .

      QMlimit(li,La) = hBar* ( WoutXE(qU,nX,La)+ WoutXE(qL,nX,La) )  ! eV

      FWevLor(li,La) = max(FWevLor(li,La), QMlimit(li,La))  ! eV; Lorentz part of future Voigt FWHM of spectral line qU - qL

      Return
      END      !  subr "LineLorWi(li)"  




      SUBROUTINE BEvPr()  ! For this (La,nX) compute Binding Energy of outermost electron "BE(k,nX,La)", 
      use mo1co2nov       ! CUT ELs with BE < 0, i.e, load their POP in next GrSt
      implicit none      

      do k= 1, NST(nX)  								
           BE(k,nX,La)= -13.d0 ! Preliminary value '-13' will reveal forgotten EL, ~20 strings below
      enddo

      do j= FSS(nX), HSS(nX)  
        do k= nuGS(j,nX), nuGS(j+1,nX)-1       ! non-AI ELs of j
          BE(k,nX,La)= PIR(j,nX,La)- E(k,nX)   ! high EL can get BE < 0 because of continuum lowering
	  enddo

        if(nuAS(j,nX).gt.0) then        
          do k= kAI1(j,nX), kAI2(j,nX)                               
                BE(k,nX,La) = 77777.    ! Mark for avoiding a cut of this AI EL;  
	    enddo                         ! AIQS have no "BE" because (by definition of AI EL) their E(k,nX) > PI 
        endif
      enddo
         BE(Nnu(nX),nX,La) = 77777.  ! nucl. Mark for avoiding cutOff by "BE.LT.1.d-3"

CHECK   "BE" and POPs < 0   
      do k= 1, NST(nX)              
        if(abs(BE(k,nX,La) +13.d0) .lt. 3.d-7) then  ! means "BE' remained initial "-13" 
	    write(*,'(2(a9,i4), a33)')
     +         'EL#', k, 'of XE#', nX, 'not given binding energy BE(k)'
		PAUSE
        endif

        if(POPi(k,nX,La).lt.-1.d-9) then
           write(*,'(a46, i2, i5)')    
     +         'In BE-check found POP < -1.d-9 in XE, k=', nX, k 
           PAUSE 'My STOP'
        endif
      enddo

CUT-off block. On this t-step some ELs could get "BE < 1.d-3". I move POP of these ELs to GrSt of next SS         
      do k= 1, NST(nX)
        if((BE(k,nX,La) .lt. 1.d-3) .AND.         ! EL got above PIR (E > PIR)
     +     (POPi(k,nX,La).gt. zero )  ) then      ! and at previous t-step had POP   
            POPi(nuGS(kiSS(k,nX)+1, nX), nX,La ) =           
     +      POPi(nuGS(kiSS(k,nX)+1, nX), nX,La ) + POPi(k,nX,La)  ! Load this POP(k) in GS of next SS
            POPi(k,nX,La)= zero                                   ! empty cut "k"
	  endif
      enddo
      Return
      END     ! of 'BEvPr' subr



      SUBROUTINE Ws()  ! Transition probabilities for Rate Equations
      use mo1co2nov    ! In each W-matrix, first index shows INITIAL EL of the transition.
      implicit none
      integer  kAI     ! mw, 
      real(8)  pv(nvL), freq, Wave, DEJ, BemHz, Bem, Bab, dev, AbSpIn, 
     +         rate, Ejm, wFac, D01ahf, EED, eeV, Sca, hvw, fun,     
     +         FWevDop, Voigt2, ArPv, SigPhi,	SiPhi, SiRR, probPhi,   
     +         probRR, probIRR, dEe, Velo 
                
      external FVSinz, FVSmi, FVStbr, FVSex, FVSdx   ! functions mentioned by name (no argument) in d01

      WI  = zero
      WEX = zero
      WDX = zero
      WRR = zero
      WTB = zero
      WInd= zero
      Wab = zero
      WPhI= zero
      WDC = zero
      WiRR= zero

c      write(*,'(a50,2i3)') 'inside subr Ws, La,nX=', La, nX

*** Probability of bound-bound INDUCED transition down (emission) and up (absorption): Wind(U,L) and Wab(L,U)
      do lw= 1, linM4                 ! loop over all spectral lines
         if( nX3(lw) .ne. nX) cycle   ! skip lines of another XE  
         kU= nUp(lw) 
         kL= nLo(lw) 
         freq= hvC(lw)/hPL    ! photon frequency, Hz
         Wave= c/freq         ! cm, Lambda
         DEJ = BolJ*hvC(lw)   ! photon energy, J

c  'Bem' is Einstein B coef for induced emission in kU --> kL
         BemHz= A(kU,kL,nX)*Wave**2/two/DEJ   ! Hz*cm2*sr/J,  A*Lam^2/2hv(J), Mih (73.9)
         Bem= BemHz*hPL                       ! eV*cm2*sr/J;  units for * SpIn [W/sr/eV/cm2] --> [W]=1/s
c  'Bab' is Einstein B coef for absorption in kL --> kU line
         Bab= Bem*g0(kU,nX)/g0(kL,nX)         ! Mih (73.8)

c  'Wind(U,L)' and 'Wab(L,U)' are B* integ{dv*p(v)*SpInEf(La,iv)};  'p(v)' is kL-->kU absorption line shape
         FWevDop= FWkVcGAU(La,nX)*hvC(lw)  ! via u3D 
         Wing   = hvC(lw)*0.3              ! Line Wing hv-Length [eV]; under-shape area will be re-norm to 1.
         ArPv   = zero
         do iv  = 2, nvM 
           pv(iv) = zero
           if(hvV(iv).GT. hvC(lw)-Wing .and. 
     +        hvV(iv).LT. hvC(lw)+Wing)     then  
                dev = abs(hvV(iv) - hvC(lw)) 
                pv(iv) = Voigt2(FWevLor(lw,La), FWevDop, hvC(lw), dev)  ! JQSRT (Sawa) Voigt. It is slower (5x ?? than Jenya's VoigtJS) but correct to 1% 
           endif   
           dhv  = hvV(iv)-hvV(iv-1) 
           ArPv = ArPv + dhv* (pv(iv)+pv(iv-1))/2.  ! Area under shape < 1 because of the line wings cut; thus part of photons lost. 
c                                                   ! To return them, the cut shape will be re-norm to Integ[pBB(v)dv] = 1
         enddo  ! iv                            
c         if(ArPv .LT. 0.5 .and. ti .GT. strt+3.*tstep) 
c     +                    write(341,'(a9, f5.1, a18, i6,f9.2)') 
c     +     'ArPv =', ArPv, '< 0.5;  lw, hvC=', lw, hvC(lw) 

         if(ArPv .LT. 0.3) cycle   ! skip this line. ArPv is small due to insufficiently fine dv/v in hv array 
	                                                   							 
         AbSpIn = zero  ! v-integral of {dv*p(v)*SpInEff(v)} [W/eV/cm2/sr]
         do iv = 2, nvM 
            dhv= hvV(iv)-hvV(iv-1)
            AbSpIn= AbSpIn + dhv*	SpInEf(La,iv)*pv(iv)/ArPv ! "ArPv" renorm no-wings under-shape area to 1. 
         enddo
         WInd(kU,kL)= Bem*AbSpIn
         Wab (kL,kU)= Bab*AbSpIn
      enddo   ! lw

c      write(*,'(a50,2i3)') 'Passed lw-loop, La,nX=', La, nX   

      upTB= 200.*Te(La)    ! Maximal energy for over-energy integration in case of Maxwell EED     
      if(bp(La).gt.1.d-7) upTB= 10*Te(La) +bc(La) +3.*bw(La)

******    One-electron Ionization-Recombination Loop:  (SS=j, k) + e  <--> (j+1, kf) + 2e
      do k= 1, NST(nX) 
        if(k .eq. Nnu(nX))       cycle  ! no ioniz of nucl, no recomb in nucl 
        if(BE(k,nX,La).LT.1.d-3) cycle  ! dead "k". It can be only non-AI "k" because Nucl and AI ELs cannot get BE < 0 by lowering of continuum
c                                       For convenience of computations I gave them BE= 77777.in subr "BEvPr" and it never changes. 
        j= KiSS(k,nX)

        do kf= 1, NST(nX)       
          if(bra(k,kf,nX).LT. 0.5)  goto 2  ! no inz/rec channel; "bra" is 0 or 1 with FAC bases 
          if(BE(kf,nX,La).LT.1.d-3) goto 2  ! dead "kf". Possible only for non-AI EL.: Nucl and AI ELs have BE= 77777, see comment 6 lines above
          if(KiSS(kf,nX).ne.j+1)    PAUSE    'SS error in WI'

          BEk= Eth(k,kf) -DPI(j,nX,La)      ! ionization energy for "(j,k) + e --> (j+1,kf) +2e"   
          if(BEk .GT. 0.99*upTB)    goto 1  ! BEk too big for e-impact ioniz at present Te: see limits of D01ahf integral below.  
c                                             Go to photo-ionization

******  Electron-impact ionization probability WI(k,kf) for transition   (SS=j, k) + e  --> (j+1, kf) + 2e, i.e. removal of ONE electron
          Ifail= -1
          rate= D01ahf(BEk,   upTB, tol, NPTS, rer, FVSinz, Nlim, Ifail)  ! cc/s 
          WI(k,kf)= rate * Dene(La)                                       ! Probability, 1/s

******  3Brecombination probability WTB(kf,k) in notations               (SS=j+1,kf) + 2e --> (SS=j, k) + e 
          rate= D01ahf(1.d-8, upTB, tol, NPTS, rer, FVStbr, Nlim, Ifail) 
          WTB(kf,k)= rate * Dene(La) 

******  PhotoIonization probability WPhI(k,kf) in notations: (SS=j, k) + hv --> (j+1, kf) + 1e
  1       probPhi = zero  
          probRR  = zero
          probIRR = zero
          do iv= 2, nvM-2  ! "-2" needed because hvV(nvM) > hvMax 
            hvw= hvV(iv)
            if(hvw .LE. BEk) cycle  ! photon is too small for photo-inz
            SiPhi	= SigPhi(hvw)
            dhv =	hvw - hvV(iv-1)
            probPhi= probPhi + dhv * FoPi* SiPhi* SpInEf(La,iv)/hvw/BolJ  ! 1/s = eV*sr*cm2*(W/cm2/sr/eV)/J 

******  RR probability WRR(kf,k):  (j+1, kf) + e --> (j,k) + hv 
            eeV = hvw - BEk
            dEe = dhv
            Velo= 5.93096887d7*sqrt(eeV)
            SiRR= SiPhi * g0(k,nX)* hvw**2 / g0(kf,nX)/eeV/1.02199815d6  ! (39); 1.022 MeV== 2mc^2, see mo1co2nov.for 9 digits 
            probRR = probRR + dEe * Dene(La)*EED(eeV) *Velo *SiRR        ! 1/s = eV* cm^-3 * (1/eV) * (cm/s) * cm2

******  Induced RR probability WiRR(kf,k)
            Sca= 5040.36264*hvw**3                        ! Planck scale 2*hv^3/c^2;  [W/cm2/sr/eV]
            fun= Velo *EED(eeV) *SiRR *SpInEf(La,iv)/Sca  ! Mihalas (74.2),(74.4)        
            probIRR= probIRR + dhv* Dene(La)* fun         ! [1/s] 
          enddo

          WPhI(k ,kf) = probPhi  ! 1/s  PhotoIonization probability
           WRR(kf,k ) = probRR   ! 1/s  RR probability; 1/s  
          WiRR(kf,k ) = probIRR  ! 1/s  Probability of Induced RR

  2       continue
        enddo      ! kf. 
  3     continue
      enddo        ! k.  Closed ioniz/recomb loop

******  Excitation Probability matrix WEX(k,kf).  Notation (j,k) --> (j, kf>k)  
      do k= 1, NST(nX) 
        if(BE(k,nX,La).LT.1.d-3) goto 5  ! dead EL. It can be only non-AI EL.: Nucl and AI ELs are "never dead" 
c                                          because I gave them BE= 77777.in subr "BEvPr" and it never changes. 
        j= KiSS(k,nX)
        do kf= 1, NST(nX)  
          if(mthdEX(k,kf).eq.-7)      goto 4  ! "-7" means that k-to-kf transition isn't described in "ExcXE...inp":  leave WEX=0
          if(KiSS(kf,nX).ne.j)        goto 4  ! another SS
          if(BE(kf,nX,La) .LT. 1.d-3) goto 4  ! skip dead "kf"
          DE= E(kf,nX) - E(k,nX)              ! because of common GSs. Remember "DE < 0" control in "SigExc" and "DE=0" control in Intro subr.    

c          write(*,'(a20, i5, f10.2, i6, 2f11.3)') 'kf, E, k, E, DE =', kf, E(kf,nX), k, E(k,nX), DE

          if(DE.le.zero) goto 4  ! no Excitation with DE <= 0
          if(DE.ge.upTB) goto 4  ! gap too large for current Te(ti)

          Ifail= -1
          rate= D01ahf(DE, upTB, tol, npts, rer, FVSEx, Nlim, Ifail)
          WEX(k,kf)= rate * Dene(La) 

******  DeExcitation Probability matrix WDX(k,kf).  Notations: (j, kf=UP) --> (j, k=LO)
          Ifail= -1
          rate= D01ahf(1.d-8, upTB, tol, npts, rer, FVSDx, Nlim, Ifail)
          WDX(kf,k)= rate * Dene(La) 
  4       continue
        enddo    ! kf
  5     continue
      enddo      ! k

******  Dielectr Capture: (j,k) + e --> (j-1,kAI).  We follow Griem3 (6.35), i.e. derive WDC from detailed balance with WAiz in high-density limit.
c                                                   Algorithm, used for "Ejm" and "WDC", is checked by exact (in LTE) Griem3 (6.34) formula  
c                                                        WDC(k,kAI) = WAiz(kAI,k)*popLTE(kAI)/popLTE(k).  
c                                                   tCR comparison showed "==" in all POPs  (but: we used "PI", not "PIR", for Ejm).
      do k= 1, NST(nX)                   ! All ELs. 
        if(BE(k,nX,La).LT.1.d-3) goto 7  ! dead EL, however, Nucl and AI ELs are "never dead" because given BE= 77777.in subr "BEvPr" and it never changes. 
        j= KiSS(k,nX)
        do kAI= Nnu(nX)+1, NST(nX)           ! check all AI ELs, maybe they are coupled to "k" by AI/DC .  
          if(WAiz(kAI,k,nX).lt. 10.) goto 6  ! NO AI/DC channel between kAI and k
	    Ejm= EAI(kAI,k) + DPI(j-1,nX,La)     ! EAI(kAI,k) > 0 is from-FAC energy of AI transition in VACUUM. Its meaning is   
c                                              E(kAI) - E(k) taken relative to common zero [which is GS of AI ion (j-1)].
c                                                                Thus EAI(kAI,k) = E(kAI) - [PI(j-1) + E(k)]. 
c                                              Plasma reduces PI to PIR thus Ejm = E(kAI) - [PI(j-1)-DPI(j-1) + E(k)] =
c                                                                                  EAI(kAI,k) + DPI(j-1,nX,La). Griem3 p.178  
          if(Ejm .le. 0.) PAUSE 'In compu WDC met Ejm < 0'

          wFac= 4.d0*(pin*a0**2*RyeV/Te(La))**1.5 
          rate= wFac* (g0(kAI,nX)/g0(k,nX)) 
     +         * WAiz(kAI,k,nX) *dexp(-Ejm/Te(La))  ! (6.35)
          WDC(k,kAI) = Dene(La) * rate              ! (6.34)
  6       continue
        enddo     ! kAI 
  7     continue
      enddo       ! k
      return
      END         ! of 'Ws' subr  



c      real(8) function K13(kR)   !  for uniform spherical plasma
c      use mo1co2nov
c      implicit none
c      real(8) kR
c      K13= Ksp(1)      ! remains for kR <= kRp(1)  
c      do i= 2, K13po
c       if(kR.gt.kRp(i-1). and. kR.le.kRp(i)) then
c	   K13= Ksp(i-1)+(Ksp(i)-Ksp(i-1))*(kR-kRp(i-1))/(kRp(i)-kRp(i-1))
c         goto 1
c       endif		      	  
c      enddo
c      if(kR.gt.kRp(K13po)) K13= Ksp(K13po) 
c  1   continue
c      end  
	      



      SUBROUTINE GauInstrConvo(Simu, Co)  ! Convolution with Gaussian instrumental function
      use mo1co2nov                       ! in interval narrower than [minGaLor, maxGaLor] [eV] 	        
      implicit none  
      integer i1 
      real(8) Simu(nvM), Co(nvM), fwPv, v0, FWins, Sver, dev, InsFunc,    
     +                                                   prF, Gauss   
      do i1= 1, nvM	 
        Co(i1)= Simu(i1)   ! Initial
        v0 = hvV(i1)
        fwPv = Ains + Bins*(v0/1.d3) + Cins*(v0/1.d3)**2  ! quadratic fit to FWHM/hv of instrumental Gaussian: 
c                                                           FWHM/hv = Ains + Bins* hv[keV] + Cins* (hv[keV])^2
        FWins= v0*fwPv   ! FWHM [eV] of instrumental Gaussian at photon energy v0.   
        if(v0 .LE. minGaLor+3.*FWins) cycle
        if(v0 .GE. maxGaLor-3.*FWins) cycle
	  Sver= zero      ! Svertka for v0
        prF = zero
        do iw= 2, nvM-1           ! loop over hv points in integral {...dv'}: convolution over v'
          if(hvV(iw) .LE. minGaLor) cycle
          if(hvV(iw) .GE. maxGaLor) cycle
          dhv= hvV(iw)- hvV(iw-1) ! eV
          dev= abs(hvV(iw) - v0)  ! eV     
          if(dev .gt. 2.5*FWins) cycle  ! dev too big:  exp[-(dev/[FWins/1.6651])^2] is too small [ < 4d-8]
          InsFunc = Gauss(FWins, dev)   ! Gaussian of known FWHM == FWins
          Sver = Sver+ (prF+ Simu(iw)*InsFunc)*dhv/2.
          prF  = Simu(iw) *InsFunc 
	  enddo
        Co(i1)= Sver 
      enddo  
      Return
      END     ! of 'GauInstrConvo' subr 



      real(8) FUNCTION Gauss(fwG, devW)  ! Gauss shape: p(v)dv= (dv/GaPa/sqpi)*exp(-[{v-v0}/GaPa]^2)
      use mo1co2nov                      ! where GAuss PArameter "GaPa" == FWHM/1.66510922
      implicit none                      ! where 1.66510922 is 2*sqrt[ln(2)];  Vains, p.248; Griem3 p.54
      real(8) fwG, devW, GaPa 
      Gauss= zero
      GaPa = fwG/1.66510922
      Gauss= dexp(-(devW/GaPa)**2)/GaPa/sqpi  ! Gaussian of known FWHM
      END


      real(8) FUNCTION Loren(fwL, devW)  ! Lorentzian shape. Griem3 (4.3) and Wolfram MathWorld
      use mo1co2nov                     
      implicit none                      
      real(8) fwL, devW
      Loren = (fwL/ToPi)/(devW**2 + (fwL/2.)**2)  ! Lorentzian of known FWHM (=fwL)
      END


      SUBROUTINE LorInstrConvo(Simu, Co)  ! Convolution with Lorentzian instrumental function
      use mo1co2nov                       ! in interval < [minGaLor, maxGaLor] [eV] 	        
      implicit none  
      integer i1 
      real(8) Simu(nvM), Co(nvM), fwPv, v0, FWins, Sver, dev, InsFunc,    
     +                                      Loren, prF, prL, ArLor   
      do i1= 1, nvM
        Co(i1)= Simu(i1)   ! Initial
        v0 = hvV(i1)
        fwPv = Ains + Bins*(v0/1.d3) + Cins*(v0/1.d3)**2  ! quadratic fit to FWHM/hv of instrumental Lorentzian.
c                                                           FWHM/hv = Ains + Bins* hv[keV] + Cins* (hv[keV])^2
        FWins= v0*fwPv     ! FWHM [eV] of instrumental Lorentzian at photon energy v0.   
        if(v0 .LE. minGaLor+ 7.*FWins) cycle  ! at hv= 5*FWHM Lorentzian is 0.01 of peak but 
        if(v0 .GE. maxGaLor- 7.*FWins) cycle  ! wings are long, therefore I normalize under-shape area to 1.
	  Sver = zero    ! Svertka for v0
        ArLor= zero    ! Area under Lorentzian with peak at v0 
        prF  = zero    ! previous value
        prL  = zero
        do iw= 2, nvM-1           ! loop over hv points in integral {...dv'}: convolution over v'
          if(hvV(iw) .LE. minGaLor) cycle
          if(hvV(iw) .GE. maxGaLor) cycle
          dhv= hvV(iw)- hvV(iw-1)
          dev= abs(hvV(iw) - v0)       
          InsFunc= Loren(FWins, dev)                  ! Lorentzian of known FWHM == FWins
          Sver = Sver + (prF+ Simu(iw)*InsFunc)*dhv/2.
          ArLor= ArLor+ (prL+ InsFunc)*dhv/2.
          prF  = Simu(iw) *InsFunc 
          prL  = InsFunc
	  enddo
        Co(i1)= Sver/ArLor  ! correction for out-of-[minGaLor, maxGaLor] wings (i.e. for under-shape area < 1) 
c        write(342,'(a30, 2f10.4, 2e11.3, f9.2)') 
c     +            'v0, ArLor, Simu, Co, dif% =', 
c     +             v0/1.e3, ArLor, Simu(i1), Co(i1), 
c     +                      (Co(i1)-Simu(i1))*100./Simu(i1)
      enddo  
c      close(342)
      Return
      END     ! of 'LorInstrConvo' subr 



      real(8) function EED(eeV)   ! [1/eV]
      use mo1co2nov
      implicit none
      real(8) eeV, EED2, EED1     
      EED1= 2.*sqrt(eeV/pin/Te(La))*exp(-eeV/Te(La))/Te(La)   ! LL Statphys p 108; norm to "under-shape area=1"
      EED2= zero
      if(bc(La)-bw(La)/2. .lt. eeV .and. eeV .le. bc(La)+bw(La)/2.) 
     +                                   EED2 = 1./bw(La)	        ! 1/eV, norm to under-shape area=1
      EED= (one- bp(La))* EED1 + bp(La) *EED2
      END



      real(8) function FVSinz(eeV)
      use mo1co2nov
      implicit none
      real(8) eeV, V, EED, SigInz     ! function "SigInz" requires nX, k, kf and assumes KiSS(kf)=KiSS(k)+1       
         V= 5.93096887d7*sqrt(eeV)    ! cm/s,  sqrt(2kE/m)
         FVSinz= EED(eeV)*V*SigInz(eeV) 
      END           



      real(8) function SigInz(eeV)  ! requires nX, k, kf and assumes KiSS(kf)=KiSS(k)+1    
      use mo1co2nov                 ! note: came here due to "bra(k,kf) > 0.5", see "WI(k,kf)=" loop; "bra" =0/1 is given in "intro" 
      implicit none
      real(8) eeV, xw, yw, OM       ! Collision Strength (Omega)

  	if(nX .NE. 1  )          PAUSE ' Came in SigInz with nX =/= 1'
      if(bra(k,kf,nX).LT. 0.5) PAUSE ' How did you get in SigInz?'    ! FAC "InzXE....inp" has no coefs for SigPhi in this channel 
  	if(eeV.LT. BEk ) then
        write(*,'(a23, 4i4, 2e15.7)') 'XE, j, k, kf, E, BEk =', 
     +                                 nX, j, k, kf, eeV, BEk 
                          PAUSE '  Came in SigInz with Ee < BEk' 
      endif
      if(j.ne.KiSS(k,nX)) PAUSE ' Came in SigInz with j =/= KiSS(k)'    ! j is input of this func
      if(k.eq.Nnu(nX))    PAUSE ' STOPped because came to ionize nucl.'

      xw= eeV/BEk                                                      
      yw= one - BEk/eeV                            ! MF Gu 
      OM = Aix(k,kf)*log(xw)+ Bix(k,kf)*yw*yw +    ! FAC guide (2.9); log(x) is natural log;
     +     Cix(k,kf)*yw/xw  + Dix(k,kf)*yw/xw/xw
      SigInz= 3.8101e-16* OM /eeV /g0(k,nX)        ! see FAC guide (2.10): "in A.U. e-imp SigInz= OM/k0^2/g(k)". 

      if(SigInz .LT. zero)       SigInz= zero        ! Avoid Sig < 0 that can happen in case of bad interpolation between FAC points 

      if(SigInz .GT. SigMax(La)) SigInz= SigMax(La)  ! (i)  We assume scaling of from-FAC ioniz cross-secs over eeV/BEk 
c                                                           but it can cause huge SigInz for "k" with small BEk.
c                                                      (ii) Also, a bad fit to from-FAC points can cause huge SigInz.
c        therefore I restrict "SigInz", "SigPhi", no need in restricting recombination because expressed via SigInz, SigPhi 
      END


      real(8) function FVStbr(eeV)  ! 3B recombination in notations   (SS=j+1,kf) + 2e --> (j,k) + e; 
      use mo1co2nov                 ! here k,kf convenient for calling ioniz'   
      implicit none                 ! function "SigInz" uses nX, k, kf, BEk from call and assumes KiSS(kf)= KiSS(k)+1   
	real(8) eeV, CroSexInz, SigTbr, V, EED, SigInz 
        	 
      CroSexInz= SigInz(eeV+BEk)  
      SigTbr= Sah(La)*CroSexInz*(eeV+BEk)*g0(k,nX)/g0(kf,nX)/eeV  ! derived from requirement of detailed balance in partial (Maxw-Saha-Boltz; no Planck) LTE

      V = 5.93096887d7*sqrt(eeV)  
      FVStbr= EED(eeV) * V * SigTbr   
      END        


 		
	real(8) function SigPhi(hv)  ! cm2, eV;  PhotoIonz Cross-Sec.   Notations:  (j,k) + hv --> (j+1,kf) + e
      use mo1co2nov                ! function "SigPhi" for nX, k, kf, BEk from call
	implicit none      
	real(8) hv, xw
  	if(nX .ne. 1  )          PAUSE 'Came in SigPhi with nX =/= 1'
      if(bra(k,kf,nX).LT. 0.5) PAUSE 'Came in SigPhi with bra < 0.5' ! FAC "InzXE....inp" has no coefs for SigPhi in this channel 
  	if(hv .LE. BEk) then
         write(*,'(/a40, 2i5, 2f12.3, e11.2)') 
     +          'In func SigPhi: k, kf, BEk, hv =',           
     +           k, kf, BEk, hv 
         PAUSE 'i.e. came in SigPhi with hv < BEk'
      endif

      xw = hv/BEk
      SigPhi= (Eix(k,kf)+ Fix(k,kf)/xw + Gix(k,kf)/xw/xw)/  ! MFGu fit to SigPhi(x), see "GU expres for Cross-sec.pdf" file from Bern   
     +                         xw**(3.5+ Hix(k,kf))   
c                              SigPhi= 7.9d-18* (BEk/hv)**3 /HSS(nX)**2  ! Kramers;  see BlackFold(42); single-e, nucl charge
      if(SigPhi.LT.zero)       SigPhi= zero        ! Bad fits to FAC points may cause Sig < 0 

      if(SigPhi.GT.SigMax(La)) SigPhi= SigMax(La)  ! (i)  We assume scaling of from-FAC ioniz cross-secs over eeV/BEk
c                                                         but it can cause huge SigPhi (if BEk is small), 
c                                                    (ii) Also, a bad fit to from-FAC points can cause huge SigPhi.
c        therefore I restrict "SigInz", "SigPhi". No need in restricting recombination because they are expressed via SigInz, SigPhi 
      END 


      real(8) function FVSEx(ev)
	implicit none
	real(8) ev, V, EED, SigExc       
        V= 5.93096887d7*sqrt(ev)
        FVSEx= EED(ev)* V* SigExc(ev)
      END


      real(8) function FVSDx(ev)   ! CALLed from "Ws", where "k" is Lower EL, "kf" is Upper EL; both of SS=j
      use mo1co2nov                ! DE== E(kf,nX) - E(k,nX)
      implicit none
      real(8) ev, V, SigDx, SigExc, EED       
        SigDx= SigExc(ev+DE)*(ev+DE)*g0(k,nX)/g0(kf,nX)/ev    ! Klein-Rosseland, Sobelman-Vainstein-Yukov, 95, p5    
        V = 5.93096887d7*sqrt(ev)
        FVSDx = EED(ev) * V * SigDx  ! Don't restrict SigDx because you restricted SigExc,
      END                            ! otherwise no transition to Boltzmann


                                          
      real(8) function SigExc(eeV)  ! 'k' is lower,  'kf' is upper
      use mo1co2nov 
      implicit none
      integer Me
      real(8) eeV, As, Bs, Cs, Ds, Es, Fs, X, X2, Sig, ALPHA, F, XN, E1
      As= Ax(k,kf)
      Bs= Bx(k,kf)
      Cs= Cx(k,kf)
      Ds= Dx(k,kf)
      Es= Ex(k,kf)
      Fs= Fx(k,kf)
      Me= MthdEX(k,kf)
      if(KiSS(k,nX) .ne. j) PAUSE 'In SigExc: wrong SS'

      if(DE.LE.zero) then   
        write(*,'(a40, 3i5,f15.4)') 'DE <= 0 in SigExc; XE, k, kf, DE=', 
     +                                                  nX, k, kf, DE
        PAUSE 
      endif 

      X= eeV/DE      
      if(X.LT.1.) then
        write(*,'(a40, 3i5, f9.4)') 'X < 1 in SigExc; XE, k, kf, DE=', 
     +                                                nX, k, kf, DE 
        PAUSE
      endif      
      X2= X*X

      SELECT CASE (Me)  ! # for fitting formula selection
        Case( 5)
          Sig= As/X +Bs/X2 +Cs/X/X2 +Ds/X2/X2 +Es*log(X)/X   ! log(x) is natural logarithm 

        Case(11)                      ! Dima Proselkov. in CODA SGM=(C+B*X1+A*X12)/(X1+D)**4/X1**E 
          if(abs(X+Ds) .lt. 1.e-6) then
             Sig = 0.
          else
             Sig= (Cs +Bs*X +As*X2)/(X+Ds)**4 /X**Es 
          endif

        Case(16)            ! a-la splines with Gauss functions 0..1 from NOMAD VB
	          ALPHA = 0.9899495D0 * DSQRT(Fs/(Fs - 1.D0)) ! numerical coefficient is 0.7 * dsqrt(2)
	          XN = DSQRT((X-1.D0)/(X+Fs))*ALPHA
	          E1 = 1.D0/Es
	          Sig = (As*DEXP(-XN*XN*E1) +
     &         Bs*DEXP(-(XN-0.333D0)**2*E1) +
     &         Cs*DEXP(-(XN-0.666D0)**2*E1) +
     &         Ds*DEXP(-(XN-1.000D0)**2*E1)) / X

      END SELECT

      if(Sig .LT. zero      ) Sig= zero        ! Bad fits to FAC points may have Sig < 0 even far from threshold 
      if(Sig .GT. SigMax(La)) Sig= SigMax(La)  ! Bad fits to FAC points may cause huge Sig

      SigExc= Sig   ! Don't restrict SigDx [expressed via Klein-Rosseland] for Boltzmann limit
      END 



      SUBROUTINE POPdot(tSC, wPOP, dPOPpdt)
      use mo1co2nov 
      implicit none
      real(8) tSC, wPOP(nQSm), dPOPpdt(nQSm), sum, xx
      do k = 1, NST(nX)
        sum = zero
        do k1 = 1, NST(nX)              ! For k1=k the PM(k,k)= -Wout(k)
          sum = sum+ wPOP(k1)*PM(k1,k)  ! for Rate Eq dPOP(k)/dt= sum(k) 
        enddo
        dPOPpdt(k)= sum/Scale           ! for slow d02
      enddo
      xx= tSC*zero   ! to avoid "tSC not used" warning
      Return
      END


    

      real(8) FUNCTION Voigt2(fwhmL, fwhmG, vC, dv) ! [eV], dv= v-vC, Sasha's connector to YR+Drayson's VOIGT(X,Y); WELL checked
	use mo1co2nov                                        
	implicit none
	real(8) fwhmL, fwhmG, vC, dv, balf, x1, y1, Voi2, VOIGT
      balf= (vC/fwhmG)*1.6651092   ! 1.6651092= 2*Sq[ln(2)]
      x1= balf*abs(dv)/vC	         ! to Dr' units
      y1= 1.665109*fwhmL/2./fwhmG  ! to Dr' units
      Voi2= VOIGT(x1,y1)/1.772454  ! with renorm from "sqrt(pi)=1.7724539; Drayson" to 1 
      Voigt2= Voi2*balf/vC	     ! to "eV"
      END


      real(8) FUNCTION VOIGT(X,Y) ! Voigt shape normalized to sqrt(pi); From Drayson, JQSRT, v.16, pp.611-614, 1976
	use mo1co2nov 
	implicit none
	integer MAX, N, MIN, Jvo
	real(8) X,Y, B(22), RI(15), XN(15), YN(15), D0(25), D1(25),D2(25),
     +       D3(25), D4(25), HN(25), XX(3),  HH(3), NBY2(19), Cv(21),
	+       CO, UU, VV, U, Y2, Hv, DXv, V
      DATA B/0.,.7093602d-7,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,
     + .0,.0,.0,.0,.0,.0/, XN/10.,9.,2*8.,7.,6.,5.,4.,7*3./,
     + YN/3*.6,.5,2*.4,4*.3,1.,.9,.8,2*.7/, Hv/.201/, XX/.5246476,
     + 1.65068,.7071068/, HH/.2562121,.2588268E-1,.2820948/, NBY2/9.5,
     + 9.,8.5,8.,7.5,7.,6.5,6.,5.5,5.,4.5,4.,3.5,3.,2.5,2.,1.5,1.,.5/,
     + Cv/.7093602E-7,-.2518434E-6,.8566874E-6,-.2787638E-5,.866074E-5,
     +  -.2565551E-4,.7228775E-4,-.1933631E-3,.4899520E-3, -.001173267,
     + .2648762E-2, -.005623190, .01119601, -.02084976, .03621573,
     + -.05851412, .08770816, -.121664, .15584, -.184, .2/
      DO I= 1,15
        RI(I)=-I/2.
      ENDDO
      DO I=1,25
        HN(I)=Hv*(I-.5)
        CO=4.*HN(I)*HN(I)/25.-2.
        DO Jvo =2,21
          B(Jvo+1)=CO*B(Jvo)-B(Jvo-1)+Cv(Jvo)
        ENDDO
        D0(I)=HN(I)*(B(22)-B(21))/5.
        D1(I)=1.-2.*HN(I)*D0(I)
        D2(I)=(HN(I)*D1(I)+D0(I))/RI(2)
        D3(I)=(HN(I)*D2(I)+D1(I))/RI(3)
        D4(I)=(HN(I)*D3(I)+D2(I))/RI(4)
      ENDDO
104   IF (X-5.) 105,112,112
105   IF (Y-1.) 110,110,106
106   IF (X.GT.1.85*(3.6-Y)) GO TO 112

      IF (Y.LT.1.45) GO TO 107
      I=Y+Y
      GO TO 108
107   I=11.*Y
108   Jvo=X+X+1.85
      MAX= XN(Jvo)*YN(I)+.46
      MIN= MIN0(16,21-2*MAX)
      
      UU=Y
      VV=X
      DO Jvo=MIN,19
        U=NBY2(Jvo)/(UU*UU+VV*VV)
        UU=Y+U*UU
        VV=X-U*VV
      ENDDO
      VOIGT=UU/(UU*UU+VV*VV)/1.772454
      RETURN
110   Y2=Y*Y
      IF (X+Y.GE.5.) GO TO 113
      
      N=X/Hv
      DXv=X-HN(N+1)
      U=(((D4(N+1)*DXv+D3(N+1))*DXv+D2(N+1))*DXv+D1(N+1))*DXv+D0(N+1)
      V=1.-2.*X*U
      
      VV=DEXP(Y2-X*X)*DCOS(2.*X*Y)/1.128379-Y*V
      UU=-Y
      MAX=5.+(12.5-X)*.8*Y
      DO I=2,MAX,2
        U=(X*V+U)/RI(I)
        V=(X*U+V)/RI(I+1)
        UU=-UU*Y2
        VV=VV+V*UU
      ENDDO
      VOIGT=1.128379*VV
      RETURN
112   Y2=Y*Y
      IF (Y.LT.11.-.6875*X) GO TO 113
      
      U=X-XX(3)
      V=X+XX(3)
      VOIGT=Y*(HH(3)/(Y2+U*U)+HH(3)/(Y2+V*V))
      RETURN
      
113   U=X-XX(1)
      V=X+XX(1)
      UU=X-XX(2)
      VV=X+XX(2)
      VOIGT=Y*(HH(1)/(Y2+U*U)+HH(1)/(Y2+V*V)+HH(2)/(Y2+UU*UU)+HH(2)/(Y2+
     1VV*VV))
      RETURN
      END     ! FUNCTION VOIGT




      subroutine PMg0() ! For known La,nX compute transition probability 
      use mo1co2nov     ! matrix PM(k,kf) and EL de-population rate Wout(k); also dZ/dt  
      implicit none 
      Wout = zero       ! Wout(k) is Total depletion rate of 'k' into all 'kf'=/='k'
c                         must be 1D for d02
      do k = 1, NST(nX)
	  if(BE(k,nX,La) .lt. 1.d-3)    goto 3  ! dead 'k'
        do kf = 1, NST(nX)                  ! to all acceptors
	    if(BE(kf,nX,La) .lt. 1.d-3) cycle   ! skip dead acceptor
	    if(kf .eq. k)               cycle   ! no self-service  

          Wout(k)= Wout(k)    +  
     1             WInd(k,kf) +  Wab(k,kf) +    A(k,kf,nX) +     ! 1/s, All probabs are k --> kf
     2               WI(k,kf) +  WTB(k,kf) + WAiz(k,kf,nX) +  
     3             WPhI(k,kf) +  WRR(k,kf) + WiRR(k,kf)    + 
     4              WDC(k,kf) +  WEX(k,kf) +  WDX(k,kf)             
        enddo  ! kf

        WoutXE(k,nX,La)= Wout(k)  ! 3D for Stark and printOuts 
        do kf = 1, NST(nX)               
          if(hBar*WoutXE(k,nX,La) .gt. abs(E(k,nX)-E(kf,nX)))             ! life-time width > DE of close levels 
     +       WoutXE(k,nX,La)= Wout(k)- A(k,kf,nX)- WEX(k,kf)- WDX(k,kf)   
        enddo
  3	  continue
	enddo      ! k

      PM= zero        !  Probability matrix PM(k,kf) is a sum over ALL k --> kf channels
      do k = 1, NST(nX)
	  if(BE(k,nX,La) .lt. 1.d-3)    cycle   ! dead 'k'
        do kf = 1, NST(nX)
	    if(BE(kf,nX,La) .lt. 1.d-3) cycle   ! skip dead acceptor
	    if(kf .eq. k)  PM(k,k) = -Wout(k)
		if(kf .ne. k)  PM(k,kf) =                             
     1                 WInd(k,kf) +  Wab(k,kf)  +    A(k,kf,nX)  ! 1/s, Probabs are k --> kf
     2               +   WI(k,kf) +  WTB(k,kf)  + WAiz(k,kf,nX)
     3               + WPhI(k,kf) +  WRR(k,kf)  + WiRR(k,kf)  
     4               +  WEX(k,kf) +  WDX(k,kf)  +  WDC(k,kf) 
        enddo   ! kf
      enddo     ! k
     	 
c  For suspicious  POP(k)*PM(k,kf) > 10^17  or  
c                  POP(k)*Wout(k)  > 10^17  print details in "Happened.dat", file (341 
      do k  = 1, NST(nX)
      do kf = 1, NST(nX)  
        if(k.eq.kf .and. Wout(k)*POPi(k,nX,La) .gt. 1.d17) then  
           write(341,'(/a9, i2, a7, f7.0, a11, i3, a7, i2, a15, e9.3, 
     +                 a15, e9.3)') 'In La=', La, 'at ti=', ti*1.d12,
     +                'ps level #', k, 'of XE=', nX,
     +                'has FluxOut =', Wout(k)*POPi(k,nX,La), 
     +                '> d17;   POP=', POPi(k,nX,La) 
c           close(341)
c           write(*,'(//a56)') 
c     +       'POP(k)*Wout(k) > 10^17; see details in "Happened".dat' 
c           PAUSE
        endif

        if(k.ne.kf .and. abs(PM(k,kf))*POPi(k,nX,La) .gt. 1.d17) then 
           write(341,'(/a9, i2, a7, f7.0, a48, i2, 2i4, a15)') 
     +        'In La=', La, 'at ti=', ti*1.d12,  
     +           'ps found |PM(k,kf)|*POPk > 10^17 for XE, k, kf=',   
     +            nX, k, kf,   ';  see details:'
           write(341,'(/a137)') 'POPk     POPkf      A        WI        
     +  WTB      WPhI      WRR       WEX       WDX       WAiz      WDC     
     +    WInd      Wab       WiRR' 
           write(341,'(19e10.3)') POPi(k,nX,La), POPi(kf, nX,La), 
     +             A(k,kf,nX),  WI(k,kf), WTB(k,kf), WPhI(k,kf), 
     +             WRR(k,kf),  WEX(k,kf), WDX(k,kf), WAiz(k,kf,nX), 
     +             WDC(k,kf), WInd(k,kf), Wab(k,kf), WiRR(k,kf)
           close(341)
           write(*,'(//a55)') 
     +	   'My STOP:  POP(k)*PM(k,kf) > 10^17; see "Happened.dat"' 
           PAUSE
        endif
      enddo
      enddo

c  Population/Depopulation details for chosen QSs of Kr in chosen zone (see "Params2.inp") at t = "tiInf"
      if(ti.LT.tiInf .and. tiInf.LE.tf .and. La.eq.LaInf) then 
        do k = 1, NST(nX)               ! consider each QS as "donor" 
          do k1= 1, nqInf               ! along array "AskInf" prescribed in "Params2.inp"
            if(k.eq.AskInf(k1)) goto 1  ! for providing this "k" with W-info		  
          enddo
          cycle      ! for this "k" info NOT requested, check next "k"

  1       write(49,'(//a7,f6.1, a13,i1, a5,i4, a9,e8.3, a55)')   
     +         'At ti =', ti*1.d12, 'ps  in zone #', La, 'QS#', k,  
     +         'has POP=', POPi(k,nX,La), 
     +         'Its main (above 5% of Max) POP-exchange rates [1/s],'
          write(49,'(a41,i4,  a8,i4,  a30,i4, a25)') 'namely, POP(', k, 
     +     ')*W(from', k, 'to kf) - POP(kf)*W(from kf to', k, 
     +                                    ') is given in 3rd column.'

          write(49,'(a110)') 'Here POP(k) is population of level k.  W(k
     +,kf) is probability of k --> kf transition due to all channels.' 
          write(49,'(a54)') 'Columns 4-15 display probability in each ch  
     +annel.'
          write(49,'(a83)') 'For each k,kf pair:  first line displays pr
     +obabilities of kf --> k transition,' 
          write(49,'(a83)') 
     +	  'second line displays probabilities of k --> kf transition.' 
          write(49,'(a62/)')   
     +       'Notation of channels is explained in manual (Section YI).' 

          write(49,'(a128)')      'kf  POP(kf)  ExchRate    A       WInd
     +     Wab      Wex      Wdx       Wi      Wphi     Waiz     WDC    
     +  WTB      WRR     WiRR'

          MaxExch= zero
          do kf = 1, NST(nX)          !  candidates for "most active partner"
            POPexch= (POPi(k,nX,La)*PM(k,kf)- POPi(kf,nX,La)*PM(kf,k))            
            if(abs(POPexch) .gt. MaxExch) MaxExch = abs(POPexch)
          enddo	
					  
          do kf = 1, NST(nX)          !  candidates for "active acceptor"
            POPexch= POPi(k,nX,La)*PM(k,kf) - POPi(kf,nX,La)*PM(kf,k)            
            if(abs(POPexch) .gt. MaxExch/20.) then

		    write(49,'(i4, 19e9.2)') kf, POPi(kf,nX,La), POPexch, 
     +                A(k,kf,nX), WInd(k,kf),  Wab(k,kf),  WEX(k,kf), 
     +              WDX(k,kf),      WI(k,kf), WPhI(k,kf), WAiz(k,kf,nX), 
     +              WDC(k,kf),     WTB(k,kf),  WRR(k,kf), WiRR(k,kf)

		    write(49,'(e31.2, 18e9.2/)') 
     +                A(kf,k,nX), WInd(kf,k),  Wab(kf,k),  WEX(kf,k), 
     +              WDX(kf,k),      WI(kf,k), WPhI(kf,k), WAiz(kf,k,nX), 
     +              WDC(kf,k),     WTB(kf,k),  WRR(kf,k), WiRR(kf,k)
            endif
          enddo   ! k1
        enddo     ! k
      endif
      return
      end subroutine  ! PMg0()




      SUBROUTINE SCENARIO()  ! load and calculate parameters for t=ti     
      use mo1co2nov     
      implicit none

      do iw = 1, ntp-1
         if(tPo(iw).LE.ti .and. ti.LT.tPo(iw+1)) i = iw 
      enddo
      frac = (ti-tPo(i)) / (tPo(i+1)-tPo(i))

      nuBSs = nuBSst(i)

      R1 = R1t(i)+ (R1t(i+1)-R1t(i))*frac  ! [cm], radius of BS
      R2 = R2t(i)+ (R2t(i+1)-R2t(i))*frac  ! [cm], radius of core
      R3 = R3t(i)+ (R3t(i+1)-R3t(i))*frac  ! [cm], radius of Capsule

      CeR(1)= R1    ! used in "write" only
      CeR(2)= R2
      CeR(3)= R3

c      diBSs = R2*(FoPi/3./nuBSs)**0.3333  ! [cm] mean distance between centers of BSs is cubic root of (core volume per BS)
c              write(*,'(a40,8e9.2)')'R1 diBSs, dc R2=', R1,diBSs, dc, R2    !@#$%************
  
      if((nuBSs-1)*nuBSs .gt. W2max*(R2/R1)**4 ) then
        write(*,'(/a75)')  'Probability of crossing two BSs > W2max, see
     + (II.1) and decrease Nbs.'  
        STOP 'My STOP'
      endif

      DenI(La)= zero	  ! total (all-XE) i/cc in the Layer
      do nX = 1, nXE	
         Den(nX,La) = nit(i,La,nX)+ (nit(i+1,La,nX)- nit(i,La,nX))*frac  ! ion number density of each XE [i/cc] at this ti in this La
         DenI(La)   = DenI(La) + Den(nX,La) 
      enddo

      do nX = 1, nXE	
         niFra(nX,La)= Den(nX,La)/DenI(La) 
      enddo

        Te(La) = Tet(i,La)+ ( Tet(i+1,La) -  Tet(i,La))*frac   !  Te [eV]   at "ti" in zone #La
      Tion(La) = Te(La)                                        !  Ti [eV] 
       u3D(La) = u3Dt(i,La)+ (u3Dt(i+1,La) - u3Dt(i,La))*frac  ! u3D [cm/s] at "ti" in this La
        bp(La) = bpt(i,La)+ (bpt(i+1,La) - bpt(i,La))*frac     !  e-beam part of EED in zone #La
        bc(La) = bct(i,La)+ (bct(i+1,La) - bct(i,La))*frac     !  central energy [eV] of e-beam in zone #La
        bw(La) = bwt(i,La)+ (bwt(i+1,La) - bwt(i,La))*frac     !  e-beam width [eV] in zone #La

      ZC1(La) = zero   ! mean ion charge in the cell: average over XE and SS (including atoms)
      DiZ2(La)= zero   ! XE-sum of Den*Z^2 for FF emission and DebR
      do nX = 1, nXE   
        ZC(nX,La)= zero
        do k= 1, NST(nX)                
          POPi(k,nX,La) = POPf(k,nX,La)                    ! load ti-POPs from "POPf"array D02-computed for previous "tf" OR loaded in Intro.   
          ZC(nX,La)= ZC(nX,La)+ POPi(k,nX,La)*(kiSS(k,nX)-1) 
        enddo
        ZC1(La) =  ZC1(La)+ niFra(nX,La) * ZC(nX,La)
        DiZ2(La)= DiZ2(La)+ Den(nX,La)* ZC(nX,La)**2
      enddo
      Dene(La) = DenI(La)*ZC1(La)  ! e/cc in the zone

      Sah(La)= Dene(La)*(one-bp(La))/6.0371288d21/Te(La)**1.5   ! ZR.674; used in 3br

      distII = one/DenI(La)**third   ! cm; mean ion-ion distance in La; 
c                  DenI(La) is total (all-XE) ion density [i/cc] in the Layer (or zone) 
      SigMax(La)= 3.14*distII**2     ! Geom Upper limit on inelastic cross-sec. 
      Return
      END     ! Scenario	   


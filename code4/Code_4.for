      use mo1code4     
      implicit none   
      CALL OpenFiles() ! Open Output files, write column titles
      CALL Intro()     ! Read input files, fill in atomic data arrays 
c                        and initial population ("POPs") of energy levels ("ELs")
      CALL LineList()  ! Find spectral lines in full [hvmin, hvmax] interval, print "LineList.dat"
      CALL hvPoints()  ! Build 1D array of hv points if full [hvMin, hvMax] interval; 
c                        re-calculate PCD response from hv-points of input file to the hv-array 	 
      tf= strt

  1   ti= tf  
      tf= ti+ tstep  
      write(*,'(/a32, f7.3, a3)') 'Start ti =', ti*1.d9, 'ns'

      if(ti.GE.StopTime) then
	   write (*,'(/a53/)') 'ti >= StopTime.  The End.'
	   STOP  
      endif

      do La = 1, LaMx     ! # of zone; 1=Core, 2=Halo     
        CALL SCENARIO()   ! for t= ti compute R, Te, u3D, DenI, ZC1, Dene
        write(*,'(a43, i2, e10.3, f7.0, e9.2, e11.4, 4f9.3)') 
     +          'Zone, R(cm), Te, u3D, ne, Z(Fe,Cr,Ni,Mn) =', 
     +           La, CeR(La), Te(La), u3D(La), Dene(La), 
     +           ZC(1,La), ZC(2,La),ZC(3,La),ZC(4,La)   

        write(30+La,'(f7.3, f7.4, f7.0, e8.2, e10.3, 5f9.4, 4e10.3,      ! files "CoreInfo.dat", "HaloInfo.dat"      
     +                      f6.3, 2f8.2)')           ti*1.d9,  CeR(La), 
     +                                       Te(La), u3D(La), Dene(La), 
     +                 ZC1(La), ZC(1,La), ZC(2,La), ZC(3,La), ZC(4,La),  ! ZC(nX,La) is mean ion charge of (nX,La), computed in Scenario  
     +                      Den(1,La), Den(2,La), Den(3,La), Den(4,La),  ! using "POPi", see "POPi(k,nX,La) = POPf(k,nX,La)" in the end of Scenario
     +                                bp(La), bc(La)/1.d3, bw(La)/1.d3   ! [keV]
		   
        do nX = 1, nXE   ! For Te(La), u3D(La), Den(nX,La) of t = ti.  
           CALL LevWi()  ! For these La,nX compu FWHM of each EL: "LvJW(La,nX,k)" [eV] 
           CALL redPI()  ! For these La,nX, for each jSS compu DPI(jSS,nX,La) and PIR(jSS,nX,La).  
           CALL BEvPr()  ! For these La,nX compu Binding Energy of the outermost electron "BE(k,nX,La)".
c                          Move POPs from BE-cut ELs to the ground state of next SpS.

           FWkVcGAU(La,nX)= 1.4757*u3D(La)/c	! Gaussian part of FWHM/hvC of each line of #nX in zone #La 
        enddo  ! nX

	  CALL EmiAbso()  ! Compu this-La "EmTot(La,hv)" [W/cc/sr/eV] and "AbTot(La,hv)" [1/cm]
c                         via params of t=ti and POPi(k,nX,La).  
      enddo  ! La       

      CALL EffSpIn()  ! Compute due-to-all-La "SpInEf(La,iv)" [W/eV/sr/cm2] in RP of each "La", 
c                       This subr has its own La-loop because each "La" is affected by other "La"s.  
c                      "SpInEf(La,iv)" is needed for computation of Win, Wab, WphI, WiRR at t= ti" for d02 of POPs towards "tf".

      CALL PowYie()   ! Compute z-pinch side-on Spectral Power;frames of TREX [file (181], PCD powers [file (59]

      do La = 1, LaMx
      do nX = 1, nXE  
                      CALL AtKins()  ! For each La,nX compu Ws and PM, run d02 for POPs from "ti" to "tf"h 
c                                      with params of "ti"; it gives "POPf(k,Xx,La)" that is POP(tf)  
c                                      to be first used in Scenario of NEXT "ti".
      enddo   
      enddo   
      goto 1  ! for starting the next t-step, where present "tf" will be "ti", thus present "POPf" will be used for compu all quantities of "ti" 
      END     ! MAIN



      SUBROUTINE OpenFiles() 
      use mo1code4
      implicit none

      open( 13, file= 'Params1.inp')
      open( 14, file= 'Params2.inp')

      open(111,file= 'QSsFe.inp')      ! Fe 
      open(112,file= 'ExcFe.inp')
      open(113,file= 'InzFe.inp')
      open(114,file= 'AIwFe.inp')

      open(211,file= 'QSsCr.inp')      ! Cr 
      open(212,file= 'ExcCr.inp')
      open(213,file= 'InzCr.inp')
      open(214,file= 'AIwCr.inp')

      open(311,file= 'QSsNi.inp')      ! Ni 
      open(312,file= 'ExcNi.inp')
      open(313,file= 'InzNi.inp')
      open(314,file= 'AIwNi.inp')

      open(411,file= 'QSsMn.inp')      ! Mn 
      open(412,file= 'ExcMn.inp')
      open(413,file= 'InzMn.inp')
      open(414,file= 'AIwMn.inp')

      open( 61, file= 'EffSpIns.dat')  ! displays [W/eV/sr/cm2] in typical point of each zone at t = "tiInf"
      write(61,'(a141)') 'hvEV      CoShoX     CoLonX     CoShoZ     CoL
     +onZ     CoreY     SpInEfCo    HaShoX     HaLonX     HaShoZ     HaL
     +onZ     HaloY     SpInEfHa'

      open(341, file= 'Happened.dat')
	 
      open(116, file= 'ZPopsFe_Core.dat')  ! population(t) of all ionization stages in Core
      open(117, file= 'ZPopsFe_Halo.dat')  !                                           Halo
      do La= 1, LaMx
         write(115+La,'(/a85)') 'tfNS    TeEV    Dene     DeniFe     Pop	 
     +zBe    PopzLi    PopzHe    PopzH    PopzNucl'	                            
      enddo

      open(31, file= 'CoreInfo.dat')
      open(32, file= 'HaloInfo.dat')
      do La= 1, LaMx
	   write(30+La,'(a146)') 'tiNS   Rcm   TeEV    u3D      Dene     Z
     +bar      Zfe      Zcr      Zni      Zmn      DenFe     DenCr     D
     +enNi     DenMn    bp    bcKeV   bwKeV'  
      enddo

      open(541, file='PIR_Fe_Core.dat')
      open(542, file='PIR_Fe_Halo.dat')
      do La= 1, LaMx
         write(540+La,'(/a30)') 'PIR/PI of Fe' 
         write(540+La,'(/a73)') 'tiNS  TeEV     Dene     DeniFe   Be-lik
     +e   Li-like   He-like    H-like'
      enddo

      open (46, file='CoreLineInfo.dat')
      open (47, file='HaloLineInfo.dat')
      do k7= 1, LaMx
        write(45+k7,'(a119)') 'Info on FWHM [eV] of spectral lines.    N
     +ote: Baranger = 0 means "Upper or Lower level is cut by continuum 
     +lowering".'
        write(45+k7,'(/a119)') 'hvcEV  XE  SpS  Lambda(A)       Upper   
     +          Lower         Gauss  Baranger  Lorentz    Voigt   PopUpp
     +er    flu'
      enddo
 
      open(181, file= 'Frames.dat')   ! Frames [J/eV/sr]. Instrum-Convolved. 
      FrYie = zero
      write(181,'(a108)') 'hvKeV     YieFr1     YieFr2     YieFr3     Yi
     +eFr4     YieFr5     YieFr6     YieFr7     YieFr8    tIntegY'    

      open(401, file= 'COREemiAbso.dat')  ! Emi and Abso details in Core
      open(402, file= 'HALOemiAbso.dat')  ! Emi and Abso details in Halo

      open(51,file='Respo5milKapPCD.inp')     ! response of PCD with 5 mils kapton filter  [1mil = 0.001 inch = 25.4 mcm]
      read(51,'(a10)') title
      do iv = 1, np5kap
        read(51,*) hv5kap(iv), Resp5kap(iv)   ! photon energy and PCD response
      enddo	  		 	  
      close(51)

      open(52,file='Respo10miKapPCD.inp')     ! response of PCD with 10 mils kapton filter
      read(52,'(a10)') title																																																																			   
      do iv = 1, np10ka
        read(52,*) hv10ka(iv), Resp10ka(iv)   ! photon energy and PCD response
      enddo	  		 	  
      close(52)

      open(53,file='Respo40miKapPCD.inp')     ! response of PCD with 40 mils kapton filter
      read(53,'(a10)') title
      do iv = 1, np40ka
        read(53,*) hv40ka(iv), Resp40ka(iv)   ! photon energy and PCD response
      enddo	  		 	  
      close(53)

       open(58, file= 'PCDrespo.dat')   ! [W]
      write(58,'(a43)') 'hvEV      RespPCD5  RespPCD10  RespPCD40'  	 

       open(59, file= 'PCDs.dat')   ! [W]
      write(59,'(a48)') 'tfNS     PCD5      PCD10     PCD40    FullPow'  	 

      Return
      END     ! "OpenFiles"



      SUBROUTINE Intro()  ! Read input files, print list of ELs (non-AI and AI), provide ELs p.q.n., A coefs and initial POPs; print this info in "Comment.dat"  
      use mo1code4 
      implicit none
      character*1 po1     ! for conversion of EL name symbols (1 position) in p.q.n., Lorb, ...
      integer char2int                      ! symbol-to-integer convertor function
      integer iniQS, nSS, mth, LL, LU, ki,  
     +        iSS1, iQS1, iSS2, iQS2,  nFi                 
      real(8) fw, AIw, trEn, Axw,Bxw, Cxw, Dxw, Exw, Fxw, Gxw, Hxw, thre   

      read(13,*) FSS(1), FSS(2), FSS(3), FSS(4)  ! "FSS" is the first Spectroscopic Symbol (SpS) of Fe, Cr, Ni, Mn in the database, e.g. 23 for Be-like Fe
      read(13,*) HSS(1), HSS(2), HSS(3), HSS(4)  ! "HSS" is SpS of H-like Fe, Cr, Ni, Mn in the database;  
      read(13,*) Nnu(1), Nnu(2), Nnu(3), Nnu(4)  ! "Nnu" is serial number of nuclei in "QSs....inp"  
      read(13,*) NST(1), NST(2), NST(3), NST(4)  ! "NST" is the number of ELs in DaBa of each chemical element   
      read(13,'(a9)') empty                      !  separating line  

c   Read "QSsXE.inp" files of Fe, Cr, Ni, Mn  
      do nX= 1, nXE        !  Fe, Cr, Ni, Mn 
        nFi= 11 + 100*nX        ! file## == 111, 211, 311, 411 are "QSsXE.inp" for Fe, Cr, Ni, Mn   
        read(nFi,'(a9)') title  ! 1st line of header  
        read(nFi,'(a9)') empty  ! 2nd line of header  

        nuGS(FSS(nX),nX)= 1     ! GS of First included SpS of this XE is given #1 in the EL list of this XE  
        do i= FSS(nX), HSS(nX)  ! all SpSs of nX, except its NUCL                
          read(nFi,*) nSS, nuQS(i,nX), nuAS(i,nX), PI(i,nX)
c          write(*,'(a30, 4i5, f10.3)') 'nX, SS, nuQS, nuAS, PI=', 
c     +                          nX, i, nuQS(i,nX), nuAS(i,nX), PI(i,nX)
          if(nSS.ne.i) then
             write(*,'(a30, 2i3)') 'nSS =/= i for XE#, SS=', nX, nSS
             write(*,'(a30, 2i3)') 'i, FSS(nX)=', i, FSS(nX)
             write(*,'(a50)')      'Re-Compile mo1code4.for'
		   PAUSE 'this is the inconsistency #1'
          endif
          if(i.gt.FSS(nX)) nuGS(i,nX)= nuGS(i-1,nX)+ nuQS(i-1,nX)  ! GS #  in full list of ELs of nX 
		
          if(nuAS(i,nX).gt.0) then 
            if(kAI1(i-1,nX).eq.0) kAI1(i,nX)= Nnu(nX) +1                  ! 1st AI EL has # = nucl# +1
            if(kAI1(i-1,nX).gt.0) kAI1(i,nX)= kAI1(i-1,nX) +nuAS(i-1,nX)  ! EL# of the 1st  AI EL of SpS=i                 
            kAI2(i,nX)= kAI1(i,nX) + nuAS(i,nX) -1                        ! EL# of the last AI EL of SpS=i
            LastAI(nX)= kAI2(i,nX)                                        
          endif
        enddo 
        nuGS(HSS(nX)+1, nX)= nuGS(HSS(nX),nX)+ nuQS(HSS(nX),nX)    ! nucl     

        if(nuGS(HSS(nX)+1, nX) .ne. Nnu(nX)) PAUSE 'Inconsistency #2'

Continue reading "QSsXE.inp". Read names of non-AI ELs and g0, E of these ELs.
        read(nFi,'(a9)') empty  ! separating line   
  1     read(nFi,*) iSS 
        do k= nuGS(iSS,nX), nuGS(iSS+1,nX)-1   ! all non-AI ELs of this SpS   
             read(nFi,'(a9,f4.0,f11.3)') QSname(k,nX), g0(k,nX), E(k,nX)
c            write( *,'(a9,f4.0,f11.3)') QSname(k,nX), g0(k,nX), E(k,nX)  

          kiSS(k,nX)= iSS 
          po1 = QSname(k,nX)(7:7)   ! (7:7) means "positions from 7 until 7" which contains p.q.n. of outer electron 
          pqn(k,nX)= char2int(po1)  ! p.q.n. of outer electron

          po1 = QSname(k,nX)(8:8)   ! orbital q.n.(s,p,d,f...) of outer electron 
          orb(k,nX)= char2int(po1)  ! orbital q.n. = 0,1,2,3, ...

          po1 = QSname(k,nX)(9:9)   ! number of equivalent electrons in nl-subshell
          eqEL(k,nX)= char2int(po1)  
c          write( *, '(4i4)') nX, pqn(k,nX), orb(k,nX), eqEL(k,nX)  
        enddo   
        if(iSS .LT. HSS(nX)) goto 1 

        read(nFi,*) nSS                  ! nucl SpS
        read(nFi,'(a9)') comme           ! nucl info
        QSname(Nnu(nX),nX)= '  nucl.  '  ! character*9
        g0(Nnu(nX),nX) = 1.               
        E(Nnu(nX),nX)= zero
        kiSS(Nnu(nX),nX)= HSS(nX)+1 
c       write( *, '(/a20, 2i4)') 'XE# nucl SS# =', nX, nSS  

Continue reading "QSsXE.inp". Read names of AI ELs and  g0, E of these ELs.
        do j= FSS(nX), HSS(nX)-1        ! Until He-like SpS (including He-like) 
c          write(*,'(a20, 3i6)') 'nX, j, nuAS =', nX, j, nuAS(j,nX)  
          if(nuAS(j,nX).lt.1) cycle
          read(nFi,'(a9)') comme        ! info line
c          write(*,'(a20)') comme  
          do k = kAI1(j,nX), kAI2(j,nX)  
             read(nFi,'(a9,f4.0,f11.3)') QSname(k,nX), g0(k,nX), E(k,nX)
c            write( * ,'(a9,f4.0,f11.3)') QSname(k,nX), g0(k,nX), E(k,nX)  
            kiSS(k,nX)= j 

            po1 = QSname(k,nX)(7:7)   ! (7:7) means "positions from 7 until 7" which contains p.q.n. of outer electron 
            pqn(k,nX)= char2int(po1)  ! p.q.n. of outer electron

            po1 = QSname(k,nX)(8:8)   ! orbital q.n.(s,p,d,f...) of outer electron 
            orb(k,nX)= char2int(po1)  ! orbital q.n. = 0,1,2,3, ...

            po1 = QSname(k,nX)(9:9)   ! number of equivalent electrons in nl-subshell
            eqEL(k,nX)= char2int(po1) 
c            write( *, '(4i4)') nX, pqn(k,nX), orb(k,nX), eqEL(k,nX)  
          enddo     ! k 
        enddo       ! j
        close(nFi)  ! "QSsXE.inp"  
      enddo         ! nX = 1, nXE

Check Yes/No equal E(k,nX) in DaBa. If YES, increase 2nd energy by 0.001 eV to avoid DE=0 in the computations
      do nX = 1, nXE
        do k = 1, LastAI(nX)-1
        do k1= k+1, LastAI(nX)
          if(KiSS(k,nX) .ne. KiSS(k1,nX)) cycle
          if(E(k,nX) .eq. E(k1,nX)) then
             write(*,'(/a16, f10.3, a14, i2,2i4)') 
     +         'Equal energies=', E(k1,nX), 'for XE k k1=', nX, k, k1
             write(341,'(/a16, f10.3, a14, i2,2i4)') 
     +         'Equal energies=', E(k1,nX), 'for XE k k1=', nX, k, k1
             E(k1,nX)= E(k1,nX)+ 0.001
             write(341,'(a20, f12.3)') 'Increased E(k1) to', E(k1,nX)
          endif		   
        enddo
        enddo
      enddo

      flu = zero  ! correct-sign (>0) Absorption Oscillator Strength 
      A   = zero  ! Einstein A coef 
      WAiz= zero  ! AutoIoniz Prob from "AIwXE...inp" 
      Ax  = zero  ! Ax to Fx are 2D coefs of the e-impact Exc Cross-Sec  
      Bx  = zero  
      Cx  = zero  
      Dx  = zero 
      Ex  = zero         
c     Fx  = zero  ! not used in excit methods 5 and 11 
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
      MthdEX =-7  ! "-7" will mark excitation channels missing in "ExcXE....inp'.  

c  Read excitation cross sections and f's from 'ExcXE.inp'. 
      do nX= 1, nXE
        nFi= 12+ 100*nX        ! "ExcXE.inp" files 112, 212, 312, 412
        read(nFi,'(a9)') title
        read(nFi,'(a9)') empty  
  6     read(nFi,*) iSS, LL, LU, mth, Axw, Bxw, Cxw, Dxw, Exw, Fxw, fw 
c                        LL, LU are serial numbers among ELs of iSS 

        if(iSS.gt.100) goto 7  ! iSS > 100 marks the end of the file

        if(LL.lt.200) kL= nuGS(iSS,nX) -1 + LL       ! "kL" is full-list serial number of "LL". Here non-AI EL thus 0 < LL < 200    
        if(LL.gt.200) kL= kAI1(iSS,nX) -1 +(LL-200)  ! "kL" is full-list serial number of "LL". Here AI EL: LL > 200                                                  AI EL:     LL > 200
	         
        if(LU.lt.200) kU= nuGS(iSS,nX) -1 + LU       ! "kU" is full-list serial number of "LU". Here non-AI EL thus 0 < LU < 200           
        if(LU.gt.200) kU= kAI1(iSS,nX) -1 +(LU-200)  !                                                   AI EL:     LU > 200
                                             
        DE= E(kU,nX) - E(kL,nX) 
        if(DE .le. zero) then
          write(*,'(a20, i3, 2(i5,f11.3))') 'nX, kL, E, kU, E=', 
     +                                   nX, kL, E(kL,nX), kU, E(kU,nX) 	  
	    PAUSE 'Excitation down in reading Exc.inp'
        endif

        MthdEX(kL,kU,nX) = mth
        Ax(kL,kU,nX) = Axw
        Bx(kL,kU,nX) = Bxw
        Cx(kL,kU,nX) = Cxw
        Dx(kL,kU,nX) = Dxw
        Ex(kL,kU,nX) = Exw

        if(fw.gt.0.) PAUSE ' Osc. Str. > 0 in ExcXE.inp' 
        flu(kL,kU,nX)= -fw   ! correct (positive) value of Absorption Oscillator Strength

        A(kU,kL,nX)= 4.339192d7*flu(kL,kU,nX)*DE**2 *g0(kL,nX)/g0(kU,nX)

        if(mth.eq. 5 .or. mth.eq.11 .or. mth.eq.0) goto 6   ! Present DaBa does not use other methods    

        write(*,'(a65,5i5)')'Excit CrosSec fit method is unknown for XE, 
	+  iSS, LL, LU, mth=', nX, iSS, LL, LU, mth
        PAUSE
  7     close(nFi)
      enddo        ! nX

Continue reading 'Params1.inp'
      do nX= 1, nXE  
        read(13,*) iniQS   ! QS## for loading initial POPs of "nX"  in zone number La  
        do La= 1, LaMx
          do k= 1, NST(nX)
            if(k.ne.iniQS) POPf(k,nX,La)= 1.d-20
            if(k.eq.iniQS) POPf(k,nX,La)= one - (NST(nX)-1)*1.d-20
          enddo
        enddo
      enddo 

      do nX= 1, nXE
        read(13,*) AtMass(nX)  ! ion mass [a.u.] for Doppler and Stark calculation of line widths
      enddo
      read(13,*) empty

      read(13,*) fluMin  ! minimal absorption oscillator strength
      read(13,*) AulMin  ! minimal Einstein A coef. Spectral lines with smaller flu or Aul 
c                        are omitted because weak vs continuum and/or noise in experimental data. 
      read(13,*) comme

      read(13,*) hvMin   ! [eV]; soft edge of hv(i) sequence of points
      read(13,*) hvMax   ! [eV]; hard edge of hv(i) sequence of points
      read(13,*) hvReso  ! the finest spectral resolution, dv/v. This finest dv/v is  reached at hv = "hvFine".   
      read(13,*) comme

        ReduReso = 4.    ! dv/v increases linearly towards "hvMin" and "hvMax" where dv/v reaches "ReduReso"*"hvReso".
      read(13,*) hvFine  ! hv [eV] at which spectral resolution is the best, here at Ly-A of Fe at 6.973175 keV

      read(13,*) hvIns1, hvIns2   ! hv-edges of applicability of instrumental function given by coefficiens Ains, Bins, Cins, 
c                                   these edges are the edges of convolution in subroutine "GauInstrConvo(Simul, Co)"
      read(13,*) Ains, Bins, Cins ! 3 coefs of quadratic fit to the instrumental function which gives
c                                   line broadening "Bro"== hv/(FWHM of instr Gaus) = Ains + Bins*hv[keV] + Cins*hv[keV]^2;
c                                           e.g. Bro=1000 gives GausFWHM = hv/1000
      read(13,*) hvPrint1, hvPrint2  ! [eV] edges of PrintOut interval of "Frames.dat" 
      close(13)

c  Read ionization cross-sec coefs from "InzXE.inp" and assign "1" to "bra(i,f,XE)" if Yes ioniz channel in "InzXE.inp"
      do nX= 1,  nXE
        nFi= 13+ 100*nX        
        read(nFi,'(a9)') title
  9     read(nFi,*) iSS1, iQS1, iSS2, iQS2, Axw, Bxw, Cxw, 
     +              Dxw, MePh, Exw, Fxw, Gxw, Hxw, thre    ! ioniz threshold

        if(iSS1 .GT.100) goto 19   ! iSS1 > 100 marks the end of the file

        if(iQS1.lt.200) ki= nuGS(iSS1,nX)-1 + iQS1      ! "ki" is full-list serial number of "iQS1". (iQS1.lt.200) means that iQS1 is non-AI        
        if(iQS1.gt.200) ki= kAI1(iSS1,nX)-1 +(iQS1-200) ! (iQS1.gt.200) means that iQS1 is AI     

        if(iQS2.lt.200) kf= nuGS(iSS2,nX)-1 + iQS2      ! "kf" is full-list serial number of "iQS2". (iQS2.lt.200) means that iQS2 is non-AI        
        if(iQS2.gt.200) kf= kAI1(iSS2,nX)-1 +(iQS2-200) ! (iQS2.gt.200) means that iQS2 is AI   

        if(kiSS(kf,nX) .ne. kiSS(ki,nX)+1) then
	     write(*,'(a42,5i5)') 'XE, iSS1, iQS1, iSS2, iQS2 =',
     +                           nX, iSS1, iQS1, iSS2, iQS2
	     write(*,'(a42,5i5)') 'XE, KiSS(ki), ki, kiSS(kf), kf =',
     +                           nX, kiSS(ki,nX), ki, kiSS(kf,nX), kf 
           PAUSE 'Error in "InzXE.inp" level numbers'
        endif

        if(mePh.eq.4) then
           bra(ki,kf,nX)= one  ! "one" means yes ionization channel (e-impact and photo) from "ki" to "kf". "bra" is either 1 or 0
        else
           write(*,'(a30, 4i6)') 'XE, ki, kf, mePh =', nX, ki, kf, mePh	! print error info
           PAUSE 'in "Inz.inp" mePh ne 4'  
        endif

        Aix(ki,kf,nX)= Axw    ! Aix-Dix are 4 coefs of e-impact ioniz cross-section (formula #4), see "InzXE.inp"
        Bix(ki,kf,nX)= Bxw  
        Cix(ki,kf,nX)= Cxw 
        Dix(ki,kf,nX)= Dxw
        Eix(ki,kf,nX)= Exw    ! Eix-Hix are 4 coefs of photo-ioniz cross-section, see "InzXE.inp"
        Fix(ki,kf,nX)= Fxw 
        Gix(ki,kf,nX)= Gxw 
        Hix(ki,kf,nX)= Hxw 
        Eth(ki,kf,nX)= thre   ! State-to-state non-reduced Ionization Threshold; in the compu it will be reduced by DPI(iSS1,nX)] 
        goto 9
  19    close(nFi)
      enddo        ! nX

c  Read AI Probabilities from 'AIw.inp'. 
      do nX= 1, nXE 
        nFi= 14+ 100*nX        ! "AIwXE.inp"  files are ## == 114, 214, 314, 414 
        read(nFi,'(a9)') title
  8     read(nFi,*) iSS1, iQS1, iSS2, iQS2, AIw, trEn	   ! AI EL to EL of next SS 
c       write(*,'(4i5, e14.6)') iSS1,iQS1, iSS2,iQS2, AIw  

        if(iSS1 .GT. 100) goto 88  ! iSS1 > 100 is the mark of the end of the file

        if(iQS1.lt.200) PAUSE 'non-AI initial EL in "AIw.inp"'
        if(abs(iQS1-200).gt.nuAS(iSS1,nX)) PAUSE'AI EL # > AIQSs in SpS' 
	   
        if(iQS1.lt.200) ki= nuGS(iSS1,nX)-1 + iQS1      ! "ki" is full-list serial number of "iQS1". (iQS1.lt.200) means that iQS1 is non-AI        
        if(iQS1.gt.200) ki= kAI1(iSS1,nX)-1 +(iQS1-200) ! (iQS1.gt.200) means that iQS1 is AI     

        if(iQS2.lt.200) kf= nuGS(iSS2,nX)-1 + iQS2      ! "kf" is full-list serial number of "iQS2". (iQS2.lt.200) means that iQS2 is non-AI        
        if(iQS2.gt.200) kf= kAI1(iSS2,nX)-1 +(iQS2-200) ! (iQS2.gt.200) means that iQS2 is AI   

        if(kiSS(kf,nX) .ne. kiSS(ki,nX)+1) then
          write(*,'(/a50)') 'In "AIwXE.inp" SS(kf) ne SS(ki)+1:' 
          write(*,'(/   4i5,   f12.3)') nX, ki, iSS1, iQS1, E(ki,nX)   
          write(*,'( i10, 2i5, f12.3)')     kf, iSS2, iQS2, E(kf,nX)  
          PAUSE 'My STOP in consistency control, reading AIw.inp' 
        endif

         EAI(ki,kf,nX)= trEn  
        WAiz(ki,kf,nX)= AIw 
        goto 8 
 88     close(nFi)
      enddo

      read(14,*) FrL    ! [s] length (duration) of a frame 
      read(14,*) strt   ! [s] start time of the run
      read(14,*) tstep  ! time step of the computation. 
              if(tstep .GT. FrL/10.) tstep = FrL/10.

      read(14,*) StopTime  
      read(14,*) tiInf       ! time to print "...LineInfo.dat", "...EmiAbso.dat", "EffSpIns.dat"  
      read(14,'(a9)') empty  

      read(14,'(a9)') comme  ! Note: centran time of 1st frame must be >= StartTime + 2*FrL.
      read(14,*) FrP     ! time [s] of centers of 8 frames from "Params2.inp"  
      read(14,*) tPo     ! t-points including 1 point before "strt" and 1 point after last frame
      if(tPo(2) .LT. strt+2.*FrL) STOP 'Move tPo(2) to > strt+2*FrL' 
      read(14,'(a9)') comme  

      read(14,*) LZt  ! length [cm] of zones in "ntp" t-points
      read(14,*) R1t  ! radius [cm] of Core  in "ntp" t-points
      read(14,*) R2t  ! radius [cm] of Halo  in "ntp" t-points
      read(14,'(a9)') empty

      do j = 1, LaMx 
         read(14,*) W1D    !  1D w-array for reading strings of Params2.inp
         do i = 1, ntp 
            nit(i,j) = W1D(i)  !  ion number density of Fe [i/cc] at t in La
         enddo
      enddo

      read(14,'(a9)') empty
      do j = 1, LaMx 
         read(14,*) W1D      
         do i = 1, ntp 
            Tet(i,j) = W1D(i)  !  t-points of Te [eV] in zone #j
         enddo
      enddo

      read(14,'(a9)') comme
      do j = 1, LaMx 
         read(14,*) W1D       
         do i = 1, ntp 
           u3Dt(i,j) = W1D(i)  !  t-points of Te [eV] in zone #j
         enddo
      enddo

      read(14,'(a9)') comme
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
      close(14)

      SpInEf = zero  ! SpInEf(La,hv) is 4pi-average RF [W/cm^2/sr/eV] for Win, Wab, WphI, WiRR. ==0 on the 1st t-step
      SpeY   = zero  ! Spectral yield [J/keV/sr] gathered since t0 until current "tf" 

c  Find "LastFr" that is number of frames before StopTime
      do k = 1, mSpe        
         if((FrP(k)+FrL/2.) .LT. StopTime) LastFr= k 
      enddo 
c     write(*,'(/a20, i3)') 'LastFr =', LastFr
      SpeY = 0.
      kfrp = 0
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
         CASE ('l')	   ! in 4l AI ELs of [He], say neon
           char2int = 0
         CASE DEFAULT
           WRITE (*,*) symbol, ' not convertible into integer'
           PAUSE 'STOPped in function char2int'
        END SELECT
      END



      SUBROUTINE LineList()   ! List spectral lines along hv;  file #30.
      use mo1code4
      implicit none
      integer lnew, lmin, nXmin ! , LoRt,UpRt
      real(8) hvCmin,	Alamda, DEul 
      open (30, file= 'LineList.dat')
      write(30,'( /a29, e7.1, a8, f6.4, a15, f4.3, a2, f5.1, a6)')   
     +    'Spectral lines with Aul >', AulMin,   ', flu >', fluMin, 
     +    'in interval (',  hvMin/1.d3, '-', hvMax/1.d3, ') keV.'   
      write(30,'(/a82/)') 'XE  SpS   hvC(eV)    Lambda(A)     A(Hz)     
     +Upper Level       Lower Level     flu'

      linM4= 0
      do nX= 1, nXE         ! Count lines between all ELs of all XE 
        lin= 0              ! # of Spectral Line found in [hvmin*1.1, hvmax]
        do k  = 1, NST(nX)  ! Lower state 
        do k2 = 2, NST(nX)  ! Upper state

          if(k .eq. k2)                 goto 1 
          if(kiSS(k,nX).ne.KiSS(k2,nX)) goto 1 ! Line not possible

          lin= lin+1    
          nLoh(lin,nX)= k      ! Lower level# prior to distributing line numbers according to their hvC
          nUph(lin,nX)= k2     ! Upper
          DEul= E(k2,nX)-E(k,nX) 
          hvCh(lin,nX)= DEul   ! Spectral line center [eV]

c         Supress weak lines
          if((flu(k, k2,nX) .LT. fluMin) .or.    
     +       (A(k2,k ,nX)   .LT. AulMin) .or.   
     +       (hvCh(lin,nX) .LT. 1.1*hvMin) .or.   ! line softer than hvMin, "*1.1"	for avoiding half-lines
     +       (hvCh(lin,nX) .GT.     hvMax)) then  ! line harder then hvMax; 
               A(k2,k,nX) = zero                  ! kill this line for shorter computation time ..
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

c  Regulate the lines according to "hvC" (thru all XE)
      do lnew = 1, linM4   ! It will be a Line# after regulation along hv (in the common line list for all XEs)
        hvCmin= 1.d7       ! [eV] arbitrary value before search
        do nX = 1, nXE
          do lw= 1, linM(nX)                  ! check all "not-weak"lines of nX 
            if(hvCh(lw,nX) .LT. hvCmin) then  ! for finding minimal hvC among all lines of all XEs
              hvCmin= hvCh(lw,nX)             ! smaller hvC found
              lmin= lw            ! remember line# of this smaller hvC
              nXmin= nX           ! remember XE#   of this smaller hvC  
            endif
          enddo
        enddo                     ! search completed
        hvC(lnew)= hvCh(lmin,nXmin)
        nLo(lnew)= nLoh(lmin,nXmin)
        nUp(lnew)= nUph(lmin,nXmin)
        nX3(lnew)= nXmin          ! XE# of this line 
        hvCh(lmin,nXmin)= 7.d7    ! excludes a possibility to pick up this line again
      enddo

c  Print common (all-XE) line list into file #30 
      do iw = 1, linM4                  
        Alamda= eVA/hvC(iw)          ! A
        write(30,'(i2, i4, f11.3, f13.4, e11.3, i6,a2,a9, i7,a2,a9,
     +              f6.3)') nX3(iw),   kiSS(nUp(iw),nX3(iw)),  
     +                    hvC(iw), Alamda, A(nUp(iw),nLo(iw),nX3(iw)), 
     +              nUp(iw), ' =',  QSname(nUp(iw),nX3(iw)),  
     +              nLo(iw), ' =',  QSname(nLo(iw),nX3(iw)), 
     +                    flu(nLo(iw),nUp(iw),nX3(iw))      
      enddo
	close(30)

      write(*,'(i7, a15)') linM4, 'spectral lines' 
      write(*,'(a22,f11.4,a4)') 'first line has hvC =',  hvC(1), 'eV.'
      write(*,'(a22,f11.4,a4)') ' last line has hvC =', hvC(linM4),'eV.' 
      Return
      END	    ! of "LineList" subr

	


      SUBROUTINE hvPoints() ! sequence of hv points provides best spectral resolution "hvReso" at "hvFine".       
	use mo1code4          ! With v-deviation from "hvFine" the resolution decreases monotonicaly to
	implicit none         ! ReduReso*"hvReso" towards "hvMin" and towards "hvMax".
      integer it, itL 

      hvV(1)= hvMin    
      do iv = 2, nvL
        ReReiv= dexp(log(ReduReso)*(hvFine-hvV(iv-1))/(hvFine-hvMin))  ! Log is ln, natural logarithm
        hvV(iv)= hvV(iv-1)*(one+ hvReso * ReReiv) 
        if(iv.eq.nvL) then
            write(*,'(/a14, i6, a15, f10.3, a9, f10.3)') 
     +           'Came to nvL=', nvL, 'but hvV(hvL) =', hvV(nvL), 
     +           ' < hvMax=', hvMax
            PAUSE 'hvV(nvL) < hvMax; incr nvL'
        endif
        nvMF= iv
        if(hvV(iv) .gt. hvFine) goto 1
      enddo
  1   continue
      do iv = nvMF+1, nvL
        ReReiv= dexp(log(ReduReso)*(hvV(iv-1)-hvFine)/(hvFine))
        if(ReReiv.gt.ReduReso) ReReiv= ReduReso 
        hvV(iv)= hvV(iv-1)*(one+ hvReso * ReReiv) 
        if(iv.eq.nvL) then
            write(*,'(/a14, i6, a15, f10.3, a9, f10.3)') 
     +           'Came to nvL=', nvL, 'but hvV(hvL) =', hvV(nvL), 
     +           ' < hvMax=', hvMax
            PAUSE 'hvV(nvL) < hvMax; incr nvL'
        endif
        nvM= iv
        if(hvV(iv) .gt. hvMax) goto 2
      enddo
  2   continue
      Write(*,'(/i40, a27)') nvM, 'points in full hv interval'
      Write(*,'(a14, i7, a12, i7)') 
     +           'Changed nvL=', nvL, 'to nvM =', nvM
      write(*,'(a51, f6.2, a12, f9.1)') 'hvV(1)=',   hvV(1), 
     +                                  'hvV(nvM)=', hvV(nvM) 
Control of consistency:
      do iv = 2, nvM 
         if(hvV(iv) .le. hvV(iv-1)) then
           write(*,'(a9,i7, a11, f14.7, a15, f14.7)') 'For iv =', iv, 
     +               'hvV(iv)=', hvV(iv),  '<= hvV(iv-1)=', hvV(iv-1)             
           PAUSE 'Error #1 in hv sequence'	  
         endif
      enddo

***   Re-calculate PCD response from hv points of input files to hv points of present computation 
      do iv= 2, nvM 
         do it=1, np5kap-1                                            ! find the response for current "iv"  
            if(hv5kap(it).lt.hvV(iv) .and. hvV(iv).le.hv5kap(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv5kap(itL))/(hv5kap(itL+1)-hv5kap(itL))		              		   
         Respo5(iv)= Resp5kap(itL)+ Frac*(Resp5kap(itL+1)-Resp5kap(itL))  ! Response of PCD with 5 mils Kapton, for "iv", 0 <= Respo <=1   

c  PCD with 10mils Kapton filter
         do it=1, np10ka-1                                              
            if(hv10ka(it).lt.hvV(iv) .and. hvV(iv).le.hv10ka(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv10ka(itL))/(hv10ka(itL+1)-hv10ka(itL))		              		   
         Resp10(iv)= Resp10ka(itL)+ Frac*(Resp10ka(itL+1)-Resp10ka(itL))  ! Response of PCD with 10mils Kapton, for current "iv"

c  PCD with 40mils Kapton filter
         do it=1, np40ka-1                                            ! find the response at current "iv"  
            if(hv40ka(it).lt.hvV(iv) .and. hvV(iv).le.hv40ka(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv40ka(itL))/(hv40ka(itL+1)-hv40ka(itL))		              		   
         Resp40(iv)= Resp40ka(itL)+ Frac*(Resp40ka(itL+1)-Resp40ka(itL))  ! Response of PCD with 40mils Kapton, for current "iv"; 0 <= Respo <=1   

      write(58,'(4f10.3)') hvV(iv), Respo5(iv), Resp10(iv), Resp40(iv)   	 
      enddo
      close(58)
      Return
      END	    ! of "hvPoints" subr



      SUBROUTINE EmiAbso()  ! For La known on CALL, Compu this-La emissivity "EmTot(La,hv)" [W/cc/sr/eV] and 
      use mo1code4          ! absorption coef corrected for stimulated emission"AbTot(La,hv)" [1/cm]            
      implicit none         ! at t = ti, using all params of t= ti and POPi(EL,nX,La)   
	          
      real(8) emisFBx(nvL,nXE), absoBFx(nvL,nXE),  ! FB,BF of each XE
     +        EmiLFul(MNLe), AbsoAm(MNLe),         ! Amplitude of bound-bound (BB) emissivity and absorptivity
     +         AlfaBB(MNLe),                       ! (nU/gU)/(nL/gL)
     +        pBB(nvL), ArPBB(MNLe), Ww, dev,           
     +        FreE, EED, FWevDop, Voigt2, alfaFB,
     +        Velo, SigPhI, contrib, SiPh, SiRR, HalfLor, VoiFWHM      

c  IMPO:  This subr and Scenario are in one La-Loop of MAIN thus thermodynamic params are just for this "La"
c  IMPO:  BUT XE-loop is closed, thus here nX= nXE+1 until nX3 in 2nd "do" loop below

c  FF  EMISSIVITY and ABSORP COEF due to scattering of ALL free electrons on ions of ALL XE. 
      do iv= 1, nvM  
         hveV = hvV(iv)
         emisFF(iv)= 1.21d-33*DiZ2(La)*Dene(La)         ! [W/cc/sr/eV], (BlackFold 61); 
     +               * exp(-hveV/Te(La))/sqrt(Te(La))   !	MAXWELLIAN EED
         absoFF(iv)= (2.40062092d-37*DiZ2(La)*Dene(La)  ! (Black F 70), Notice 1% correction in constant
     +                  / sqrt(Te(La))/hveV**3 )        ! to fit emiFF/absoFF= 2hv/Lamb^2 in Planck.
     +                     * (one- exp(-hveV/Te(La)))   ! Corre for stimulated emission in Maxw case. 
      enddo

c  This-La full-shape BB EMISSIVITY and ABSORPTIVITY due to all lines;  Also Line Lorentz width "FWevLor(lw,La)" 
      do lw= 1, linM4      ! line# runs along hvC(lw) indepe of XE
        EmiLFul(lw)= zero  ! In-Full-Shape Emissivity, W/cc/sr
        AbsoAm(lw) = zero  ! Amplitude of Absorp Coef (16) except for *P(hv); i.e. [eV/cm], see (18)

        if(hvC(lw) .LT. 1.1*hvMin) PAUSE 'Line with hvC < hvMin'  ! "1.1" is reserved for red wing of soft lines (at least some part of their wings) 
        if(hvC(lw) .GT.     hvMax) PAUSE 'Line with hvC > hvMax'

        kU= nUp(lw)
        kL= nLo(lw)
        nX= nX3(lw)                  ! nX3(iw) is XE# of this line 
        if(KiSS(kU,nX).ne.KiSS(kL,nX)) PAUSE 'Confusion in SS of kU, kL'

        if(kU.LT.Nnu(nX) .AND. BE(kU,nX,La).LT. 1.d-3) goto 1  ! skip line from non-AI dead EL
        if(kL.LT.Nnu(nX) .AND. BE(kL,nX,La).LT. 1.d-3) goto 1  ! skip line onto non-AI dead EL

        if(POPi(kU,nX,La) .LT. 1.d-9)                  goto 1  ! weak line

        EmiLFul(lw)= BolJ*hvC(lw)*A(kU,kL,nX)*              ! In-Full-Shape Emissivity, W/cc/sr
     +                    POPi(kU,nX,La)*Den(nX,La)/FoPi  
        AlfaBB(lw)= zero
        if(POPi(kL,nX,La).gt.1.d-12) AlfaBB(lw)=
     +    (POPi(kU,nX,La)/g0(kU,nX))/(POPi(kL,nX,La)/g0(kL,nX))

***      POPs inversion can cause AlfaBB(lw) < 0 thus causes AbsoAm(lw) < 0 BUT 
***      SpIn remains > 0 because Abso < 0 causes (Emi/Kap) < 0  and  [1 - exp(-kL)] < 0.

        AbsoAm(lw)= 1.09760936d-16*flu(kL,kU,nX)*                ! Absorptivity (16) except "*P(hv)"; i.e. [eV/cm], see (18)
     +              POPi(kL,nX,La)*Den(nX,La)*(one-AlfaBB(lw)) 
  1     continue
     
        CALL LineLorWi(lw)   ! eV; Compu "FWevLor(lw,La)" that is Lorentz FWHM of line from "kU" to "kL"

        FWevDop= FWkVcGAU(La,nX)*hvC(lw)   ! GAU via u3D (that is due to thermal+hydro motions)
        HalfLor= FWevLor(lw,La)/2.
        VoiFWHM= HalfLor +sqrt(HalfLor**2 + FWevDop**2)  		  

        if(ti.LT.tiInf .and. tiInf.LE.tf) then           ! at print-time "tiInf" trint in "...LineInfo.dat"
           write(45+La,'(f9.3, 2i4, f12.4, i6,a2,a9, i7,a2,a9, f9.3,   
     +                                  2f9.2, f10.2, e11.3, f7.3)')  
     +                    hvC(lw), nX3(lw),  kiSS(kU,nX), eVA/hvC(lw),  	
     +             kU,' =',QSname(kU,nX),      kL,' =',QSname(kL,nX), 	
     +                   FWkVcGAU(La,nX)*hvC(lw),                     
     +                          QMlimit(lw,La), FWevLor(lw,La), VoiFWHM,
     +                                     POPi(kU,nX,La), flu(kL,kU,nX) 
           if(lw.eq.linM4) close(45+La)
        endif
      enddo        ! lw loop

      absoBB= zero     ! local plasma bound-bound absorption coef corre for stimulated emission [1/cm], XE-sum, line sum
      emisBB= zero     ! local plasma BB emissivity   [W/cc/sr/eV]
      do lw= 1, linM4  ! each "hvV(iv)" may include contributions from many lines because of their long wings.
         nX = nX3(lw)    
         kU = nUp(lw)
         kL = nLo(lw)

         FWevDop= FWkVcGAU(La,nX)*hvC(lw)  ! for this line, via u3D
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

         if(ArPBB(lw) .LT. 1.d-2) then
           write(*,'(/a36, i6, f10.3, 9e10.2)') 
     +       'lw, hvC, FWevDop, ArPBB(lw) =', 
     +        lw, hvC(lw), FWevDop, ArPBB(lw)
	     PAUSE '  ArPBB(lw) < 0.01)' 
         endif

         do iv= 1, nvM                             
           emisBB(iv)= emisBB(iv)+ EmiLFul(lw) *pBB(iv) ! BB emissivity [W/cc/sr/eV] DUE TO ALL lines of this XE, at hvV(iv)
     +                                       /ArPBB(lw) ! return photons from cut wings  
           absoBB(iv)= absoBB(iv)+ AbsoAm(lw)  *pBB(iv) ! [eV/cm]*[1/eV] = 1/cm  (16) DUE TO ALL LINES
     +                                       /ArPBB(lw) ! return photons from cut wings  
         enddo     
      enddo    ! lw

******  BF absorptivity corrected for induced inverse process and FB emissivity, 
***                     Notations: (j,k) + hv --> (j+1, kf) + e , like in function "SigPhi" to be used
      do iv= 1, nvM 
        do nX = 1, nXE
          absoBFx(iv,nX)= zero  ! contribution of one nX
          emisFBx(iv,nX)= zero  
          do k = 1, NST(nX)  
             if(POPi(k,nX,La) .LT. 1.d-10)  goto 7  ! Negligible contribution    
             j= KiSS(k,nX)
             do kf= 1, NST(nX)                       
               if(POPi(kf,nX,La).LT.1.d-10) goto 6
               if(bra(k,kf,nX)  .LT. 0.5  ) goto 6  ! no channel; "bra" is 0 or 1 with FAC bases 
               if(KiSS(kf,nX)   .ne. j+1  )  PAUSE 'Check j in BF abso'

               BEk= Eth(k,kf,nX) - DPI(j,nX,La)     ! min hv required for (j,k)+hv --> (j+1,kf)+e photo-ionization
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
               absoBFx(iv,nX)= absoBFx(iv,nX) + contrib                 ! (50), this XE

c  FB EMISSION due to RR onto ions of current nX:  (SS=j+1,kf) + e  --> (j,k) + hv   
		  	 
               Velo= 5.93096887d7*sqrt(FreE)               ! Velocity of free electron = sq(2k*E/m); cm/s  
               SiRR= SiPh *g0(k ,nX)* hvV(iv)**2 /        
     +                    (g0(kf,nX)* FreE* 1.02199815d6)  ! (39); 1.022 MeV== 2mc^2, see mo1code4.for 9 digits 

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
	  
      do nX = 1, nXE
         do iv= 1, nvM                   
            emisFB(iv) = emisFB(iv) + emisFBx(iv,nX)   ! W/cc/sr/eV;  
            absoBF(iv) = absoBF(iv) + absoBFx(iv,nX)   ! 1/cm
         enddo  ! iv    
      enddo     ! nX

      do iv= 1, nvM  	
         emTot(La,iv)= emisBB(iv)+ emisFF(iv)+ emisFB(iv)  ! W/cc/sr/eV;                                     
         abTot(La,iv)= absoBB(iv)+ absoFF(iv)+ absoBF(iv)  ! 1/cm 
      enddo 

      if(ti.LT.tiInf .and. tiInf.LE.tf) then  ! after processing all XE, at print-info time "tiInf" print this-La "...EmiAbso.dat"
	  write(400+La,'(a98)') 
     +  'hvKeV    emisFB     emisBB     emisFF     emiTot     absoBF
     + absoBB     absoFF    absoTot'    	
        do iv= 1, nvM  
           write(400+La,'(f11.4, 19e11.4)')  hvV(iv)/1.d3,    
     +        emisFB(iv), emisBB(iv), emisFF(iv), emTot(La,iv),   
     +        absoBF(iv), absoBB(iv), absoFF(iv), abTot(La,iv) 
        enddo 
        close(400+La)
      endif
      Return
      END     ! of 'EmiAbso' subr



      SUBROUTINE EffSpIn()  ! Space- direction- average RF "SpInEf(La,hv)" [W/eV/sr/cm2] for  WInd, Wab, WPhI and WiRR
      use mo1code4          ! "SpInEf(La,hv)" is computed for one RP of each La using one typical LOS     
      implicit none
      real(8) Sp1c(nvL), Sp2c(nvL), Sp3c(nvL), Sp4c(nvL), Sp5c(nvL), 
     +        Sp1h(nvL), Sp2h(nvL), Sp3h(nvL), Sp4h(nvL), Sp5h(nvL),
     +        ab1, ab2, ta1, ta2, So1, So2, pathC, pathH1,pathH2  

********  I estimate "SpInEf(La,iv)" using mean over 6 _|_ directions.
********     La=1 is Core. I choose RP at r= R1/2,    y=0, z=Lz/4.
********     La=2 is Halo. I choose RP at r= R1+R2/2, y=0, z=Lz/4.

      pathC  = sqrt(R1**2 - (R1/2.)**2)          ! Along-y path in Core towards RP of Core 
      pathH1 = sqrt(R2**2 - (R1/2.)**2) - pathC  ! Along-y path in Halo towards RP of Core 
      pathH2 = sqrt(R2**2 - (R1+(R2-R1)/2.)**2)  ! Along-y path in Halo towards RP of Halo 

c      write(341,'(/29e10.3)') Lz, R1, R2, pathC, pathH1, pathH2   	  	 

	do iv= 1, nvM
        ab1 = abTot(1,iv)   ! in-Core absorption coef (corrected for induced emission) [1/cm]
        ta1 = ab1*R1  	
        So1 = zero                             
        if(abs(ab1) .gt. 1.d-20) So1 = emTot(1,iv)/ab1   ! [W/eV/sr/cm2]. Excluded /0. Notice: "ab1" can be negative (POPs inversion)
c                                                          but "SpIn" remains > 0 because ab1 < 0 makes [1-dexp] < 0 	 
        ab2 = abTot(2,iv)   ! Halo around Core
        ta2 = ab2*(R2-R1)  	
        So2 = zero                             
        if(abs(ab2) .gt. 1.d-20) So2 = emTot(2,iv)/ab2   ! [W/eV/sr/cm2]. Excluded /0; see comment to So1 
	  
c        write(341,'(29e10.3)') ab1, emTot(1,iv), So1, ta1, hvV(iv) 
c        write(341,'(29e10.3)') ab2, emTot(2,iv), So2, ta2   	  	 

********************   La=1 is Core. RP at r= R1/2, y=0, z=Lz/4.

        Sp1c(iv)= So2*(1.-exp(-ta2))*exp(-ta1*0.5) +           ! On short along-x-axis path to RP of Core 
     +            So1*(1.-exp(-ta1*0.5))  
        Sp2c(iv)= So2*(1.-exp(-ta2))*exp(-ta1*1.5) +           ! On long ........... 
     +            So1*(1.-exp(-ta1*1.5))
        Sp3c(iv)= So1*(1.-exp(-ab1*Lz*0.25))                   ! Short along-z-axis LOS thru Core. no Halo along central LOS
        Sp4c(iv)= So1*(1.-exp(-ab1*Lz*0.75))                   ! Long  along-z-axis.............. ........
        Sp5c(iv)= So2*(1.-exp(-ab2*pathH1))*exp(-ab1*pathC) +  ! LOS || to y-axis 
     +            So1*(1.-exp(-ab1*pathC)) 

        SpInEf(1,iv)= (Sp1c(iv) + Sp2c(iv) + Sp3c(iv) +        ! [W/eV/sr/cm2]. La=1 is Core. 
     +                 Sp4c(iv) + 2.*Sp5c(iv) )/6.  

c        write(341,'(9e10.3)') exp(-ta1*0.5), exp(-ta1*1.5),  
c     +	                                   exp(-ab1*Lz*0.25) 
c        write(341,'(9e10.3)') Sp1c(iv), Sp2c(iv), Sp3c(iv), Sp4c(iv), 
c     +                                  Sp5c(iv), SpInEf(1,iv), So1

*******************   La=2 is Halo. I choose RP at r= R1+R2/2, y=0, z=Lz/4.

        Sp1h(iv)= So2*(1.-exp(-ta2/2.))                       ! Short LOS along x-axis 
        Sp2h(iv)= So2*(1.-exp(-ta2))*exp(-2.*ta1 -ta2/2.)	+   ! Long  LOS along x-axis; term 1 is due to far Halo
     +            So1*(1.-exp(-2.*ta1))*exp(-ta2/2.) +        !                         term 2 is due to Core           
     +            So2*(1.-exp(-ta2/2.))                       !                         term 2 is due to near half-Halo 
        Sp3h(iv)= So2*(1.-exp(-ab2*Lz*0.25))   ! Short parallel to z-axis LOS thru Halo, no Core along this LOS 
        Sp4h(iv)= So2*(1.-exp(-ab2*Lz*0.75))   ! Long  .................. 
        Sp5h(iv)= So2*(1.-exp(-ab2*pathH2))    ! LOS along y-axis 

        SpInEf(2,iv)= (Sp1h(iv) + Sp2h(iv) + Sp3h(iv) +  ! [W/eV/sr/cm2]. La=2 is Halo 
     +                 Sp4h(iv) + 2.*Sp5h(iv))/6. 
	
c        write(341,'(9e10.3)') exp(-ta2*0.5), exp(-ta2*1.5),  
c     +	                                   exp(-ab2*Lz*0.25) 
c        write(341,'(9e10.3)') Sp1h(iv), Sp2h(iv), Sp3h(iv), Sp4h(iv), 
c     +                                  Sp5h(iv), SpInEf(2,iv), So2

*******************
      enddo   ! iv
***                  Note: my compiler shows "Fatal Error" if I put any print inside previous "do" loop
***                        therefore I had to write all "Sp1c"....."Sp5h" as arrays 
***                        and write next "do" loop for printing them:  

      if(ti.LT.tiInf .and. tiInf.LE.tf) then    ! at single print-info time "(tiInf") print 13 columns of "EffSpIns.dat"
        do iv= 1, nvM					    
          write(61,'(f9.3, 19e11.4)')   hvV(iv),                           ! [eV]  
     +      Sp1c(iv),Sp2c(iv),Sp3c(iv),Sp4c(iv), Sp5c(iv), SpInEf(1,iv),	 ! W/eV/sr/cm2]
     +      Sp1h(iv),Sp2h(iv),Sp3h(iv),Sp4h(iv), Sp5h(iv), SpInEf(2,iv)
        enddo  
        close(61)
      endif
      Return		  
      END     ! 'EffSpIn' subr



      SUBROUTINE PowYie()  ! is CALLed at each t for computation of radiation power "RadPow(hv)" 
c                          to be used in simulation of signals of three PCDs. 
c                      During 8 time intervals (of "FrL" duration prescribed in "Params2.inp")
c                      the "RadPow" is collected in "FrYie(iv,#)" [J/keV/sr] and    
c                      printed in file "Frames.dat" within hv-interval of TREX frames.
c                      For comparison to experiment: each "FrYie(iv,#)" must be multiplied by
c                      spectral response of TREX. 
c                      Full-time radiation yield [J/keV/sr] is printed in the last column of "Frames.dat"
      use mo1code4       
      implicit none  ! Note: this subr is CALLed from outside La-loop, therefore on the entry La = LaMx+1 	
      real(8)        Ww(nvL), Wconv(nvL), yc(10), So1,So2, pathH, pathC,      
     +               SpInHC, SpInCH, SpInOut, Pow5, Pw10, Pw40, FuPow,
     +                                              exwH, exwC, ArStr  
      ArStr = Lz* R2/10. ! [cm2] area of y-strip on plane surface x-bright towards side-on detector 
   
*************************   Side-on Radiation Power.[W/eV/sr] 
      do k = 1, 10
         yc(k) = (R2/10.)*k - R2/20.  ! y-coordinate of center of k-th y-strip at y > 0.
      enddo                           ! i.e. along Upper half of cylindric plasma

      do iv= 1, nvM
         RadPow(iv) = zero               ! [W/eV/sr]
         So1 = zero                        ! [W/eV/sr/cm2]
         if(abs(abTot(1,iv)) .gt. 1.d-20)  ! exclude "/0." in next line;  La=1 is Core,  
     +      So1 = emTot(1,iv)/abTot(1,iv)  ! in case of POPs inversion: "abTot" can be < 0  (alfa > 1) but 
c                                            "SpOut" remains > 0 because [1-dexp...] is also negative
         So2 = zero
         if(abs(abTot(2,iv)) .gt. 1.d-20)  ! exclude "/0." La=2 is Halo,  
     +      So2 = emTot(2,iv)/abTot(2,iv)  ! in case of POPs inversion... see comme to So1

         do k = 1, 10                            ! 10 strips at y > 0         
            if(yc(k) .ge. R1) then               ! LOS path thru Halo only   
               pathH = 2.*sqrt(R2**2-yc(k)**2)
               exwH  = dexp( -abTot(2,iv)* pathH )   
               RadPow(iv) = RadPow(iv) + So2*(1.-exwH) *ArStr  ! [W/eV/sr]  
            else
               pathC = 2.*sqrt(R1**2-yc(k)**2)
               pathH =    sqrt(R2**2-yc(k)**2) - pathC/2.  ! path thru Halo on ONE side of the Core
               exwH  = dexp( -abTot(2,iv)* pathH )   
               exwC  = dexp( -abTot(1,iv)* pathC )   
               
               SpInHC = So2*(1.-exwH)                ! [W/eV/sr/cm2] from Halo to Core interface along k-th towards-detector LOS
               SpInCH = SpInHC*exwC + So1*(1.-exwC)  !               from Core to Halo ........ 
               SpInOut= SpInCH*exwH + So2*(1.-exwH)  !               from Halo towards detector along k-th LOS

               RadPow(iv) = RadPow(iv)+ ArStr* SpInOut ! [W/eV/sr] towards the detector along k-th LOS  
            endif
         enddo    ! k 
         RadPow(iv) = 2.*RadPow(iv) ! [W/eV/sr] "*2" means "+ the same from y < 0"
      enddo  ! iv

*****    Signals [W] of 3 side-on PCDs calculated based on power radiated along 1 side-on LOS [W/sr], multiplied by 4pi [sr], like in shot 1520 

      Pow5 = zero  ! at-this-t Power [W] shown by PCD with  5 mils Kapton filter
      Pw10 = zero  !                                       10
      Pw40 = zero  !                                       40   
      FuPow= zero	 ! Full (no filters, no PCD response curves) Radiation Power from z-pinch, calculated based on power radiated along 1 side-on LOS [W/sr], multiplied by 4pi [sr]  

      do iv= 2, nvM 
         dhv= hvV(iv)-hvV(iv-1)
         Pow5  = Pow5 + RadPow(iv)*Respo5(iv)*dhv *FoPi  ! [W] = [W/eV/sr] *1 *[eV] *[sr]       
         Pw10  = Pw10 + RadPow(iv)*Resp10(iv)*dhv *FoPi  
         Pw40  = Pw40 + RadPow(iv)*Resp40(iv)*dhv *FoPi  
         FuPow = FuPow+ RadPow(iv)           *dhv *FoPi																																										   
      enddo  

      write(59,'(f8.3, 7e10.3)') tf*1.e9, Pow5, Pw10, Pw40, FuPow  ! [ns], [W] 

Collect Radiation Yield [J/eV/sr]	during proper frame 
c      do k = 1, LastFr                              
c         if((FrP(k)-FrL/2.).LE.tf .and. tf.LT.(FrP(k)+FrL/2.)) then ! tf is inside Frame #k 
c            do iv = 1, nvM 
c               FrYie(iv,k)= FrYie(iv,k) + tstep* RadPow(iv)       ! [J/eV/sr] Radiation Yield during the frame
c            enddo
c         endif
c      enddo  

Collect Radiation Yield during proper frame [J/eV/sr]
      do kfr = 1, LastFr                              
        if((FrP(kfr)-FrL/2.).LE.ti .and. ti.LT.(FrP(kfr)+FrL/2.)) then   ! ti is inside Frame #kfr
          if(kfr.ne.kfrp) then  ! new frame
		   CoFr = 0
             kfrp = kfr
          endif
          CoFr= CoFr+1
          write(*,'(a30, 2i3)') 'Frame#, +# =', kfr, CoFr
          do iv = 1, nvM 
            FrYie(iv,kfr)= FrYie(iv,kfr) + tstep*RadPow(iv)  ! [J/eV/sr] Radiation Yield during the frame
          enddo                                              ! Note: RadPow of ti, see above 
        endif
      enddo   
	  
Collect full-time radiation yield [J/eV/sr] since t0 till StopTime  
      do iv = 1, nvM 
         SpeY(iv)=  SpeY(iv) + tstep*RadPow(iv)  ! [J/eV/sr]                     
      enddo

      if(tf.GE.StopTime) then  ! Last visit to subr "PowYie"	
        do k = 1, LastFr
          do iv = 1, nvM 
             Ww(iv)= FrYie(iv,k)    ! [J/eV/sr]  
          enddo		    
		Wconv = zero              
          CALL GauInstrConvo(Ww, Wconv)  ! Convolution with (Gaussian) instrumental function 
          do iv = 1, nvM 
             FrYie(iv,k)= Wconv(iv) ! [J/eV/sr]     
          enddo  ! iv				              
        enddo	   ! k

	  do iv = 1, nvM 
           Ww(iv)= SpeY(iv)    ! [J/eV/sr]  
        enddo		    
        Wconv = zero              
        CALL GauInstrConvo(Ww, Wconv)  ! Convolution with (Gaussian) instrumental function 
        do iv = 1, nvM 
           SpeY(iv)= Wconv(iv) ! [J/eV/sr]     
        enddo  ! iv				              

        do iv = 1, nvM		
           if(hvPrint1.LE.hvV(iv) .and. hvV(iv).LE.hvPrint2)             ! hv-interval for printing frame
     +        write(181,'(f10.6, 19e11.4)') hvV(iv)/1.e3,                ! [keV]   
     +            FrYie(iv,1)*1.e3, FrYie(iv,2)*1.e3, FrYie(iv,3)*1.e3,  ! [J/keV/sr] Energy radiated by during one frame. Each "FrYie(iv) is
     +            FrYie(iv,4)*1.e3, FrYie(iv,5)*1.e3, FrYie(iv,6)*1.e3,  !          . convolved with instrumental gaussian in [hvCon1, hvCon2] interval   
     +            FrYie(iv,7)*1.e3, FrYie(iv,8)*1.e3,                    !            Here "*1.e3" for [J/eV/sr] to [J/keV/sr]
     +                              SpeY(iv)*1.e3                        ! [J/keV/sr] Radiation Yield gathered since t0 till "StopTime"; convolved with instrumental function.  
        enddo                                                          
      endif  

      Return 					  
      END     ! of 'PowYie' subr



      SUBROUTINE AtKins()  ! For La,nX known from the CALL, run Atomic Kinetics from "ti" to "tf"   
      use mo1code4         ! using params, W's, POPs  of t=ti [these POPs are POPi(k,nX,La)]. 
      implicit none        ! "Atkins" produces POPf(k,nX,La), that are POPs at t = tf 
c                            to be first used on next t-step as POPs of "ti".      						  
      real(8)  Sum1     
      external POPdot  ! subroutine mentioned by name in arguments of NAG d02

      CALL Ws()  ! calculate all transition probabilities mentioned in the Rate Equations
      CALL PMg0  ! PM(k,kf), Wout(k), WoutXE(k,nX,La) for g0 case

      do k= 1, NST(nX)
        POP(k) = POPi(k,nX,La)   ! Load POPs in 1D array. 1D is request of d02 
      enddo

c  Integrate the rate equations:
      ifail= -1
      Scale= 1.d40
      tiS= ti*Scale
      tfS= tf*Scale

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

      if(abs(SumP-one) .gt. 1.d-6) then 	     
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
c                                Scenario of next "ti" == present "tf" for proper "La" and "XE" 

      do k= 1, NST(nX)
        POPf(k,nX,La)= POP(k)	 ! save as 'tf" value for loading in POPs in Scenario for next ti
      enddo

Calculate POPZ(SS) of nX passed thru d02  
      do j= FSS(nX), HSS(nX)+1
        POPZ(j,nX,La)= zero      ! relative abundance of ionization stage
      enddo
      do k= 1, NST(nX)
        POPZ(KiSS(k,nX),nX,La)= POPZ(KiSS(k,nX),nX,La) + POP(k) 
      enddo

      do j= FSS(1), HSS(1)+1                                ! all SS of nX = 1
         if(POPZ(j,1,La) .lt. 1.d-40) POPZ(j,1,La)= 1.d-40  ! FOR ORIGIN processing 
      enddo
      if(nX.eq.1)  ! print for Fe only
     +write(115+La,'(f7.3, f8.1, 20e10.4)') tf*1.d9, Te(La), Dene(La),     
     +                         Den(1,La),           POPZ(HSS(1)-3,1,La), 
     +                         POPZ(HSS(1)-2,1,La), POPZ(HSS(1)-1,1,La), 
     +                         POPZ(HSS(1)  ,1,La), POPZ(HSS(1)+1,1,La)
      Return
      END   ! of 'AtKins' subr




      SUBROUTINE redPI()  ! For this La and nX=1 this subr gives from-the-ground-state  
      use mo1code4        ! ionization energy of ions (PIR) in the Ion-Sphere approach           
      implicit none                                    
      real(8) SpheR       

      do jSS = FSS(nX), HSS(nX)                       ! all ionizible SSs; jSS is spectroscopic symbol    
        SpheR= ( 3.*(jSS-1)/FoPi/Dene(La) )**third    ! [cm], jSS-1 is the charge of ion, see Manual (A.2)         
        DPI(jSS,nX,La)= 2.*RyeV *jSS *a0/SpheR        ! [eV], Manual (A.5). 
        PIR(jSS,nX,La)= PI(jSS,nX) - DPI(jSS,nX,La)         
        if(PIR(jSS,nX,La) .LT. PI(jSS,nX)/2.)	PAUSE '  PIR too low'
      enddo    

      if(nX.eq.1)  ! nX=1 is Fe, thus print only for Fe 
     +   write(540+La,'(f7.2, f7.0, 20e10.4)') ti*1.d9, Te(La),    
     +                          Dene(La), Den(1,La), 
     +                          PIR(HSS(1)-3,1,La)/PI(HSS(1)-3,1),
     +                          PIR(HSS(1)-2,1,La)/PI(HSS(1)-2,1),
     +                          PIR(HSS(1)-1,1,La)/PI(HSS(1)-1,1), 
     +                          PIR(HSS(1)  ,1,La)/PI(HSS(1)  ,1) 
      Return
      END     ! of 'redPI' subr




      SUBROUTINE LevWi()  ! For La,nX from the CALL, compute FWHM of all ELs "LvJW(La,nX,k)" [eV] following Refs [8,9]. 
      use mo1code4        ! Also compu "LvLorW(La,nX,k)" [eV] that is life-time-limited "LvJW(La,nX,k)"; 
      implicit none  

c  in this subr e = m = (h/2pi) = 1; for conversion to CGS units see http://en.wikipedia.org/wiki/Atomic_units 
c  the energy   unit is 1 hartree = 27.211396 eV = 2*Ry = 4.359810e-18 J; "har" in code
c  the length   unit is 1 bohr    = 0.52917725e-8 cm; in the code == a0
c  the velocity unit is c/137.036 = 2.187691e+08 cm/s NIST CODATA; electron velo on the 1st Bohr orbit
c                    or c*Alfa  ,   where Alfa = 1/137.036 is the fine structure constant of Sommerfeld (1916)
c  the E-lield  unit is   e/a0^2  = 5.14220652e9 V/cm 

      TeAU = Te(La)/har     ! 1 hartree = 27.211396 eV
c      TiAU = Tion(La)/har  But no "Tion" in this code, only u3D dueto thermal+hydro motion of ions           
      TiAU = 2*Te(La)/har   ! assumed Tion(La) ~ 2*Te(La), like found in shots 1520, 1907          

      DeAU = Dene(La)*1.48185e-25  ! Dene [e/cc] * a0^3 [cm^3/AU of volume] = e/(AU of volume) 
      do iX= 1, nXE                       ! types of perturbing IONs, including "radiator"   
        DiAU(iX)= Den(iX,La)*1.48185e-25 
         WSr(iX)= (CoSp/DiAU(iX))**third              ! Bohr, JS (3.1) dist betw ions of same XE (Wigner-Seitz radius)   
         Deb(iX)= sqrt(TiAU/FoPi/DiAU(iX))/ZC(iX,La)  ! Bohr, JS (3.4) Debye radius via ions of only one XE  
         HoF(iX)= 2.6031*ZC(iX,La)*DiAU(iX)**0.6667   ! Holtzmark field due to ions of one XE; JS (3.49); 2.60311 = 2pi(4/15)^2/3 
      enddo                                           ! add electrons to the perturbers:

      WSr(nXE+1)= (CoSp/DeAU)**third    ! dist betw electrons; J (3.1)   
      HoF(nXE+1)= 2.6031*DeAU**0.6667   ! due-to electrons Holtzmark field (3.49)
      Deb(nXE+1)= sqrt(TeAU/FoPi/DeAU)  ! electron Debye radius; JS (3.4) 
      DeF(nXE+1)=        Deb(nXE+1)                     ! "Full" Debye radius JS (3.5) for electrons equals to "pure-e" because no lighter particles      
      DeF(4)= 1./sqrt(1./Deb(nXE+1)**2 + 1./Deb(4)**2)  ! "4" is Mn; Full Debye radius JS (3.5) for Mn is Sum over e and Mn

      DeF(3)= 1./sqrt(1./Deb(nXE+1)**2 + 1./Deb(4)**2   ! "3" is Ni. For Ni .... is Sum over e, Mn, Ni   
     +                                 + 1./Deb(3)**2) 

      DeF(2)= 1./sqrt(1./Deb(nXE+1)**2 + 1./Deb(4)**2   ! "2" is Cr. For Cr ...  is Sum over e, Mn, Ni, Cr
     +                  + 1./Deb(3)**2 + 1./Deb(2)**2) 

      DeF(1)= 1./sqrt(1./Deb(nXE+1)**2 + 1./Deb(4)**2   ! "1" is Fe. For Fe .... is Sum over e, Mn, Ni, Cr, Fe
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
      fre(nXE+1,nX)= sqrt(vel(nXE+1)**2 + vel(nX)**2)/WSr(nXE+1)  ! microfield frequency due to perturbing electrons JS (3.50); 

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
        enddo  ! kw

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

        LvLorW(La,nX,k)= max(LvJW(La,nX,k), hBar* WoutXE(k,nX,La))  ! eV
      enddo   ! k 
      Return
      END     ! of 'LevWi' subr



      SUBROUTINE LineLorWi(li)  ! Compute Lorentz FWHM of spectral Line "FWevLor(li,La)"; to be used for Voigt line shape. 
      use mo1code4               
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




      SUBROUTINE BEvPr()  ! For this La,nX compu Binding Energy of outermost electron "BE(k,nX,La)", 
      use mo1code4        ! CUT ELs with BE < 0, i.e, load their POP in next GrSt
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



      SUBROUTINE Ws()  ! For (La,nX) known from "CALL AtKins": compute this-nX transition probabilities
      use mo1code4     ! In each W-matrix, first index shows INITIAL EL of the transition.
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

*** Probability of bound-bound INDUCED emission and absorption: Wind(U,L) and Wab(L,U)
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
         AbSpIn = zero                        ! integral { dv*p(v)*SpInEf(v) }

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
         enddo  ! iv                                ! see "/ArPv" below
	                                                   
         if(ArPv .LT.1.d-5) then				   
            write(*,'(a26, i6, f10.3, 9e10.2)') 
     +      'lw, hvC, FWevDop, ArPv =', 																																																		 
     +       lw, hvC(lw), FWevDop, ArPv
	      PAUSE '  ArPv < d-5)' 
         endif

         AbSpIn = zero  ! v-integral of {dv*p(v)*SpInEff(v)} [W/eV/cm2/sr]
         do iv = 2, nvM 
            dhv= hvV(iv)-hvV(iv-1)
            AbSpIn= AbSpIn + dhv*	SpInEf(La,iv)*pv(iv)/ArPv ! "ArPv" renorms no-wings under-shape area to 1. 
         enddo
         WInd(kU,kL)= Bem*AbSpIn
         Wab (kL,kU)= Bab*AbSpIn
      enddo   ! lw

      upTB= 200.*Te(La)    ! Upper limit for energy integration    
      if(bp(La).gt.1.d-7) upTB= 200.*Te(La) +bc(La) +6.*bw(La)

******    One-electron Ionization-Recombination Loop:  (SS=j, k) + e  <--> (j+1, kf) + 2e
      do k= 1, NST(nX) 
        if(BE(k,nX,La).LT.1.d-3) goto 3  ! dead EL. It can be only non-AI because Nucl and AI ELs are "never dead" 
c                                        To avoid de-POP, I gave them BE= 77777.in subr "BEvPr" and it never changes. 
        j= KiSS(k,nX)

        do kf= 1, NST(nX)      
           if(bra(k,kf,nX).LT. 0.5)  goto 2  ! no inz/rec channel; "bra" is 0 or 1 with FAC bases 
          if(k .eq. Nnu(nX)) PAUSE ' bra(k,kf,nX) allowed ioniz of nucl' 

          if(BE(kf,nX,La).LT.1.d-3) goto 2  ! dead "kf". Possible only for non-AI EL.: Nucl and AI ELs are "never dead" To avoid de-POP, I gave them BE= 77777.in subr "BEvPr" and it never changes
          if(KiSS(kf,nX).ne.j+1)    PAUSE    'SS error in WI'

          BEk= Eth(k,kf,nX) -DPI(j,nX,La)   ! ionization energy for "(j,k) + e --> (j+1,kf) +2e"   
          if(BEk .GT. 0.99*upTB)    goto 1  ! BEk too big for e-impact ioniz at present Te: see limits of D01ahf integral below.  
c                                            Go to photo-ionization

          if(BEk .LT. 1.) write(*,'(/a20, i4)') 'BEk < 1eV for k=', k

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
            SiRR= SiPhi * g0(k,nX)* hvw**2 / g0(kf,nX)/eeV/1.02199815d6  ! (39); 1.022 MeV== 2mc^2, see mo1code4.for 9 digits 
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
          if(mthdEX(k,kf,nX).eq.-7)   goto 4  ! "-7" means that k-to-kf transition of nX isn't described in "ExcXE.inp": leave WEX=0
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
        do kAI= Nnu(nX)+1, NST(nX)            ! check all AI ELs, maybe they are coupled to "k" by AI/DC .  
          if(WAiz(kAI,k,nX).lt. 10.) goto 6   ! NO AI/DC channel between kAI and k
	    Ejm= EAI(kAI,k,nX) +DPI(j-1,nX,La)  ! EAI(kAI,k) > 0 is from-FAC energy of AI transition in VACUUM. Its meaning is   
c                                             E(kAI) - E(k) taken relative to common zero [which is GS of AI ion (j-1)].
c                                                           Thus EAI(kAI,k) = E(kAI) - [PI(j-1) + E(k)]. 
c                                             Plasma reduces PI to PIR thus Ejm = E(kAI) - [PI(j-1)-DPI(j-1) + E(k)] =
c                                                                           EAI(kAI,k) + DPI(j-1,nX,La). Griem3 p.178  
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



      real(8) function EED(eeV)   ! [1/eV]
      use mo1code4
      implicit none
      real(8) eeV, EED2, EED1  ! , T2, Gw   
      EED1= 2.*sqrt(eeV/pin/Te(La))*exp(-eeV/Te(La))/Te(La)     ! LL Statphys p 108  
      EED2= zero
      if(bc(La)-bw(La)/2. .lt. eeV .and. eeV .le. bc(La)+bw(La)/2.) 
     +                                   EED2 = bp(La)/bw(La)	            ! 1/eV
      EED= (one- bp(La))* EED1 + bp(La) *EED2
      END



      real(8) function FVSinz(eeV)
      use mo1code4
      implicit none
      real(8) eeV, V, EED, SigInz     ! function "SigInz" requires nX, k, kf and assumes KiSS(kf)=KiSS(k)+1       
         V= 5.93096887d7*sqrt(eeV)    ! cm/s,  sqrt(2kE/m)
         FVSinz= EED(eeV)*V*SigInz(eeV) 
      END           



      real(8) function SigInz(eeV)  ! requires nX, k, kf and assumes KiSS(kf)=KiSS(k)+1    
      use mo1code4                  ! note: came here due to "bra(k,kf) > 0.5", see "WI(k,kf)=" loop; "bra" =0/1 is given in "intro" 
      implicit none
      real(8) eeV, xw, yw, OM       ! Collision Strength (Omega)

      if(bra(k,kf,nX).LT. 0.5) PAUSE ' How did you get in SigInz?'    ! FAC "InzXE....inp" has no coefs for SigPhi in this channel 
  	if(eeV.LT. BEk ) then
        write(*,'(a23, 4i4, 2e15.7)') 'XE, j, k, kf, E, BEk =', 
     +                                 nX, j, k, kf, eeV, BEk 
                          PAUSE '  Came in SigInz with Ee < BEk' 
      endif
      if(j.ne.KiSS(k,nX)) PAUSE ' Came in SigInz with j =/= KiSS(k)'    ! j is input of this func
      if(k.eq.Nnu(nX))    PAUSE ' STOPped because came to ionize nucl.'

      xw= eeV/BEk                                                      
      yw= one - BEk/eeV                                  ! MF Gu 
      OM = Aix(k,kf,nX)*log(xw)+ Bix(k,kf,nX)*yw*yw +    ! FAC guide (2.9); log(x) is natural log;
     +     Cix(k,kf,nX)*yw/xw  + Dix(k,kf,nX)*yw/xw/xw
      SigInz= 3.8101e-16* OM /eeV /g0(k,nX)              ! see FAC guide (2.10): "in A.U. e-imp SigInz= OM/k0^2/g(k)". 

      if(SigInz .LT. zero)       SigInz= zero        ! Avoid Sig < 0 that can happen in case of bad interpolation between FAC points 

      if(SigInz .GT. SigMax(La)) SigInz= SigMax(La)  ! (i)  We assume scaling of from-FAC ioniz cross-secs over eeV/BEk 
c                                                           but it can cause huge SigInz for "k" with small BEk.
c                                                      (ii) Bad fit to from-FAC points can cause huge SigInz.
c           therefore I restrict "SigInz", "SigPhi", no need in restricting recombination because expressed via SigInz, SigPhi
      END

      real(8) function FVStbr(eeV)  ! 3B recombination in notations   (SS=j+1,kf) + 2e --> (j,k) + e; 
      use mo1code4                  ! here k,kf convenient for calling ioniz'   
      implicit none                 ! function "SigInz" uses nX, k, kf, BEk from call and assumes KiSS(kf)= KiSS(k)+1   
	real(8) eeV, CroSexInz, SigTbr, V, EED, SigInz 
        	 
      CroSexInz= SigInz(eeV+BEk)  
      SigTbr= Sah(La)*CroSexInz*(eeV+BEk)*g0(k,nX)/g0(kf,nX)/eeV  ! derived from requirement of detailed balance in partial (Maxw-Saha-Boltz; no Planck) LTE

      V = 5.93096887d7*sqrt(eeV)  
      FVStbr= EED(eeV) * V * SigTbr   
      END        


 		
	real(8) function SigPhi(hv)  ! cm2, eV;  PhotoIonz Cross-Sec.   Notations:  (j,k) + hv --> (j+1,kf) + e
      use mo1code4                 ! function "SigPhi" for nX, k, kf, BEk from call
	implicit none      
	real(8) hv, xw
      if(bra(k,kf,nX).LT. 0.5) PAUSE 'Came in SigPhi with bra < 0.5' ! FAC "InzXE....inp" has no coefs for SigPhi in this channel 
  	if(hv .LE. BEk) then
         write(*,'(/a40, 2i5, 2f12.3, e11.2)') 
     +          'In func SigPhi: k, kf, BEk, hv =',           
     +           k, kf, BEk, hv 
         PAUSE 'i.e. came in SigPhi with hv < BEk'
      endif

      xw = hv/BEk
      SigPhi= (Eix(k,kf,nX)+ Fix(k,kf,nX)/xw + Gix(k,kf,nX)/xw/xw)/    ! MFGu fit to SigPhi(x), see "GU expres for Cross-sec.pdf" file from Bern   
     +                                         xw**(3.5+ Hix(k,kf,nX))   
c                              SigPhi= 7.9d-18* (BEk/hv)**3 /HSS(nX)**2  ! Kramers;  see BlackFold(42); single-e, nucl charge
      if(SigPhi.LT.zero)       SigPhi= zero        ! Bad fits to FAC points may cause Sig < 0 

      if(SigPhi.GT.SigMax(La)) SigPhi= SigMax(La)  ! (i)  We assume scaling of from-FAC ioniz cross-secs over eeV/BEk
c                                                         but it can cause huge SigPhi (if BEk is small), 
c                                                    (ii) Bad fit to from-FAC points can cause huge SigPhi.
c        therefore I restrict "SigInz", "SigPhi". No need in restricting recombination because they are expressed via SigInz, SigPhi 
      END 


      real(8) function FVSEx(ev)
	implicit none
	real(8) ev, V, EED, SigExc       
        V= 5.93096887d7*sqrt(ev)
        FVSEx= EED(ev)* V* SigExc(ev)
      END


      real(8) function FVSDx(ev)   ! CALLed from "Ws", where "k" is Lower EL, "kf" is Upper EL; both of SS=j
      use mo1code4                 ! DE== E(kf,nX) - E(k,nX)
      implicit none
      real(8) ev, V, SigDx, SigExc, EED       
        SigDx= SigExc(ev+DE)*(ev+DE)*g0(k,nX)/g0(kf,nX)/ev    ! Klein-Rosseland, Sobelman-Vainstein-Yukov, 95, p5    
        V = 5.93096887d7*sqrt(ev)
        FVSDx = EED(ev) * V * SigDx  ! Don't restrict SigDx because you restricted SigExc,
      END                            ! otherwise no transition to Boltzmann

                                          
      real(8) function SigExc(eeV)  ! 'k' is lower,  'kf' is upper
      use mo1code4
      implicit none
      integer Me
      real(8) eeV, As, Bs, Cs, Ds, Es, X, X2, Sig, Gaunt, ALPHA, F, XN,
     + E1 ! Vstavil Fs VB
      As= Ax(k,kf,nX)
      Bs= Bx(k,kf,nX)
      Cs= Cx(k,kf,nX)
      Ds= Dx(k,kf,nX)
      Es= Ex(k,kf,nX)
      Me= MthdEX(k,kf,nX)
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
        Case( 0)        ! Van Regemoter  .  
		 Gaunt= .349*log(X)+ .0988+ .455/X               ! Actually, for "n --> n'" in H-like ions 
           Sig = Ampli * abs(flu(k,kf,nX)) *Gaunt /X/DE/DE  

        Case( 5)
           Sig= As/X +Bs/X2 +Cs/X/X2 +Ds/X2/X2 +Es*log(X)/X   ! log(x) is natural logarithm 

        Case(11)                          ! Proselkov. in CODA SGM=(C+B*X1+A*X12)/(X1+D)**4/X1**E 
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

      if(Sig .LT. zero)       Sig= zero        ! Bad fits to FAC points may have Sig < 0 even far from threshold 
      if(Sig .GT. SigMax(La)) Sig= SigMax(La)  ! Bad fits to FAC points may cause huge Sig

      SigExc= Sig
      END 



      SUBROUTINE POPdot(tSC, wPOP, dPOPpdt)
      use mo1code4
      implicit none
      real(8) tSC, wPOP(nQSm), dPOPpdt(nQSm), sum, xx
      do k = 1, NST(nX)
        sum = zero
        do k1 = 1, NST(nX)              ! For k1=k the PM(k,k)= -Wout(k)
          sum = sum+ wPOP(k1)*PM(k1,k)  ! for Rate Eq dPOP(k)/dt= sum(k) 
        enddo
        dPOPpdt(k)= sum/Scale           ! for slow-time d02
      enddo
      xx= tSC*zero   ! to avoid "tSC not used" warning
      Return
      END


    
      SUBROUTINE GauInstrConvo(Simul, Co)  ! Convolution with Gaussian instrumental function
      use mo1code4                         ! at hvIn1 <= hv <= hvIn2	        
      implicit none  
      integer i1 
      real(8) Simul(nvL), Co(nvL), Bro, v0, FWin, Sver, dev, FuIns,    
     +                                                  Gauss, prF   
      Co= Simul       ! Initial; at least, "Simul" will remain non-Convo 
      do i1= 1, nvM
        v0 = hvV(i1)
        if(v0.LE.hvIns1) cycle
        if(v0.GE.hvIns2) cycle
        Bro= Ains + Bins*(v0/1.d3) + Cins*(v0/1.d3)**2  ! quadratic fit to instrumental broadening "Bro"== hv/(FWHM of instrum Gauss)
c 	  .   											  e.g. Bro = 1000 means that FWHM of Instr Gaussian = hv/1000 
        FWin= v0/Bro    ! FWHM [eV] of instr Gaussian. By the above definition of "Bro".  
	  Sver= zero      ! Svertka for v0
        prF = zero
        do iw= 2, nvM-1           ! loop over hv points for integral {...dv'}: convolution over v'
          dhv= hvV(iw)- hvV(iw-1)
          dev= abs(hvV(iw) - v0)       ! hv' - hv
          if(dev .gt. 2.5*FWin) cycle  ! this point is too far from v0:  exp[-(dev/[FWHM/1.6651])^2] is too small [ < 4d-8]
          FuIns= Gauss(FWin, dev)      ! Gaussian of known FWHM == FWin
          Sver = Sver+ (prF+ Simul(iw)*FuIns)*dhv/two
          prF  = Simul(iw) *FuIns 
	  enddo
        Co(i1)= Sver 
      enddo  
      Return
      END     ! of 'GauInstrConvo' subr 




      real(8) FUNCTION Voigt2(fwhmL, fwhmG, vC, dv) ! [eV], dv= v-vC, Sasha's connector to YR+Drayson's VOIGT(X,Y); WELL checked
	use mo1code4                                       
	implicit none
	real(8) fwhmL, fwhmG, vC, dv, balf, x1, y1, Voi2, VOIGT
      balf= (vC/fwhmG)*1.6651092   ! 1.6651092= 2*Sq[ln(2)]
      x1= balf*abs(dv)/vC	         ! to Dr' units
      y1= 1.665109*fwhmL/2./fwhmG  ! to Dr' units
      Voi2= VOIGT(x1,y1)/1.772454  ! with renorm from "sqrt(pi)=1.7724539; Drayson" to 1 
      Voigt2= Voi2*balf/vC	     ! to "eV"
      END


      real(8) FUNCTION VOIGT(X,Y) ! Voigt shape normalized to sqrt(pi); From Drayson, JQSRT, v.16, pp.611-614, 1976
	use mo1code4
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




      real(8) FUNCTION Gauss(fwG, devW)  ! Gauss shape: p(v)dv= (dv/GaPa/sqpi)*exp(-[{v-v0}/GaPa]^2)
      use mo1code4                       ! has FWHM= 2*sqrt[ln(2)]*GaPa= 1.66510922*GaPa;  Vains, p.248; Griem3 p.54
      implicit none                      ! where GAuss PArameter "GaPa" == FWHM/1.66510922
      real(8) fwG, devW, GaPa 
      Gauss= zero
      GaPa = fwG/1.66510922d0
      Gauss= dexp(-(devW/GaPa)**2)/GaPa/sqpi  ! Gaussian of known FWHM
      END



      subroutine PMg0()  ! For  (La,nX) known from "CALL AtKins", compu transition probability 
      use mo1code4       ! matrix PM(k,kf) and EL de-population rate Wout(k)  
      implicit none 
      Wout = zero        ! Wout(k) is Total depletion rate of 'k' into all other 'kf'
c                          must be 1D for d02
      do k = 1, NST(nX)
	  if(BE(k,nX,La) .lt. 1.d-3)    goto 3  ! dead 'k'
        do kf = 1, NST(nX)                    ! to all acceptors
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
     +       WoutXE(k,nX,La)= Wout(k)- A(k,kf,nX)- WEX(k,kf)- WDX(k,kf)   ! for mixed (k,kf) up/down lost sense 
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
     +                 a15, e9.3)') 'In La=', La, 'at ti=', ti*1.d9,
     +                'ns level #', k, 'of XE=', nX,
     +                'has FluxOut =', Wout(k)*POPi(k,nX,La), 
     +                '> d17;   POP=', POPi(k,nX,La) 
c           close(341)
c           write(*,'(//a56)') 
c     +       'POP(k)*Wout(k) > 10^17; see details in "Happened".dat' 
c           PAUSE
        endif

        if(k.ne.kf .and. abs(PM(k,kf))*POPi(k,nX,La) .gt. 1.d17) then 
           write(341,'(/a9, i2, a7, f7.0, a48, i2, 2i4, a15)') 
     +        'In La=', La, 'at ti=', ti*1.d9,  
     +           'ns found |PM(k,kf)|*POPk > 10^17 for XE, k, kf=',   
     +            nX, k, kf,   ';  see details:'
           write(341,'(a137)')  'POPk     POPkf      A        WI        
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
      return
      end subroutine  ! PMg0()



      SUBROUTINE SCENARIO()  ! for La on CALL, calculate Den(nX,La), Deni(La), Te(La) u3D(La), ZC1(La), Dene(La) for t=ti     
      use mo1code4           
      implicit none

      do iw = 1, ntp-1
         if(tPo(iw).LE.ti .and. ti.LT.tPo(iw+1)) i = iw 
      enddo
      frac = (ti-tPo(i)) / (tPo(i+1)-tPo(i))

      R1 = R1t(i)+ (R1t(i+1)-R1t(i))*frac  !  cm, radius of core
      R2 = R2t(i)+ (R2t(i+1)-R2t(i))*frac  !  cm, radius of halo
      Lz = LZt(i)+ (LZt(i+1)-LZt(i))*frac  !  cm, length of Core, Halo
      CeR(1)= R1                           !  for printouts only
      CeR(2)= R2                           !  .................

      Den(1,La) = nit(i,La)+ (nit(i+1,La)- nit(i,La))*frac  ! ion number density of Fe [i/cc] at this ti in this La

c     I use chemical composition of stainless steel 304L, see Ref [13] in "Manual 3" in folder "Code 3 as 3zone SS of Aug2024".
c     This alloy has (by mass) 71.6% Fe, 18.3% Cr, 8.20% Ni, 1.47% Mn. I neglect 0.5% Mo, 0.023% C, ... 
c     Mss grams of alloy contain N(Fe)= Mss*0.716/mi(Fe) ions of Fe; here mi(Fe)== Uam*55.85 is the mass of an ion
c                                N(Cr)= Mss*0.183/mi(Cr)         Cr       mi(Cr)== Uam*52.00 
c     As Cr and Fe are in same volume, ratio of ion number densities n(Cr)/n(Fe) = N(Cr)/N(Fe) = 
c                  = (0.183/52.00)/(0.716/55.85) = 0.275   
c      n(Ni)/n(Fe) = (0.082/58.69)/(0.716/55.85) = 0.109  
c      n(Mn)/n(Fe) = (0.015/54.94)/(0.716/55.85) = 0.021  

      Den(2,La) =	Den(1,La)*0.275
      Den(3,La) =	Den(1,La)*0.109
      Den(4,La) =	Den(1,La)*0.021

      DenI(La)  = Den(1,La)*(1. +0.275 +0.109 +0.021) 

      do nX = 1, nXE	
         niFra(nX,La)= Den(nX,La)/DenI(La) 
      enddo

        Te(La) = Tet(i,La)+ ( Tet(i+1,La) -  Tet(i,La))*frac   ! Te [eV]   at "ti" in zone #La
c      Tion(La) = Te(La)                                       ! Ti [eV]. However, I don't compute, don't use "Ti" in this code   
c                         because the assumption of Ti=Te is not reasonable for z-pinches; also the e-energy eq cannot be used
c                         because estimate of radiation transfer between core and halo will be of low accuracy. 
c                         One the other hand,if I use Te and u3D [thermal+hydro], I have only one computation where I really need Ti(t),
c                         this is J-Stark via J-Y paper. There I replaced Ti by 2Te like found in 1520, 1907, see "TiAU = 2*Te(La)/har"     

       u3D(La) = u3Dt(i,La)+ (u3Dt(i+1,La) - u3Dt(i,La))*frac  ! u3D [cm/s] at "ti" in this La
        bp(La) = bpt(i,La)+ (bpt(i+1,La) - bpt(i,La))*frac     ! e-beam part of EED in zone #La
        bc(La) = bct(i,La)+ (bct(i+1,La) - bct(i,La))*frac     ! central energy [eV] of e-beam in zone #La
        bw(La) = bwt(i,La)+ (bwt(i+1,La) - bwt(i,La))*frac     ! e-beam width [eV] in zone #La

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
c                  DenI(La) is total (all-XE) ion density [i/cc] in the Layer
      SigMax(La)= 3.14*distII**2     ! Geom Upper limit on ionization cross-sec, see comment of Oct 16, 2020. 

      Return
      END     ! Scenario	   


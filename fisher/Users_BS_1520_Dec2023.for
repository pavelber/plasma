      use mo1          ! Feb 14, 2024.
      implicit none

      CALL OpenFiles() ! Open Output files & write the column titles
      CALL Intro()     ! Read input files; produce atomic data arrays & initial POPs
      CALL LineList()  ! Find spectral lines possible in [hvSmo, hvmax] domain & print hv-reguladed LineList

	write(*,'(a50)') 'tf[ns]  D[mm]   Te[eV]  TD[keV]    i/cc     %Mg'  

      write(32,'(a88)') 'tfNS   Dmm   TeEV   TiEV  TDkeV    niAL      ni
     +MG    ZbarAL  ZbarMG    Dene        u3D'  

******************  Build the Grid of t-points
      gpEq= 0  ! integer array of Six ## of t-grid points at which t = tres(j), j = 1,..., 6
      npa = 20 ! number of t-points between tres(j) and tres(j+1) 
      tgp(1) = strt  ! first t-point of the t-grid 
      do kw = 2, 50      ! t-point# in the t-grid
         if(tgp(kw-1) .LT. tres(1)) then 
	      tstep = (tres(1) - strt)/npa  
            tgp(kw) = tgp(kw-1) + tstep     
            Du= (tgp(kw)-tres(1)) / tres(1)      ! deviation  from tres(1)
            if( abs(Du) .LT. 1e-8 ) gpEq(1)= kw  ! "gpEq)" is NUMBER of t-point at which t = tres()  	
         endif	
         if(gpEq(1) .GT. 0) goto 1   
      enddo

 1    continue
      do kw = gpEq(1)+1, gpEq(1)+50
         if(tgp(kw-1) .LT. tres(2)) then 
	      tstep = (tres(2) - tres(1))/npa  
            tgp(kw) = tgp(kw-1) + tstep     
            Du= (tgp(kw)-tres(2)) / tres(2)
            if( abs(Du) .LT. 1.e-8 ) gpEq(2)= kw 	
         endif	
         if(gpEq(2) .GT. 0) goto 2   
      enddo	     	   
		     	   
 2    continue
      do kw = gpEq(2)+1, gpEq(2)+50
         if(tgp(kw-1) .LT. tres(3)) then 
	      tstep = (tres(3) - tres(2))/npa  
            tgp(kw) = tgp(kw-1) + tstep     
            Du= (tgp(kw)-tres(3)) / tres(3)         ! deviation from  tres(3)
            if( abs(Du) .LT. 1.e-8 ) gpEq(3)= kw 	
         endif	
         if(gpEq(3) .GT. 0) goto 3  
      enddo	     	   

 3    continue
      do kw = gpEq(3)+1, gpEq(3)+50
         if(tgp(kw-1) .LT. tres(4)) then 
	      tstep = (tres(4) - tres(3))/npa  
            tgp(kw) = tgp(kw-1) + tstep     
            Du= (tgp(kw)-tres(4)) / tres(4)         ! deviation from  tres(4)
            if( abs(Du) .LT. 1.e-8 ) gpEq(4)= kw 	
         endif	
         if(gpEq(4) .GT. 0) goto 4  
      enddo	     	   

 4    continue
      do kw = gpEq(4)+1, gpEq(4)+50
         if(tgp(kw-1) .LT. tres(5)) then 
	      tstep = (tres(5) - tres(4))/npa  
            tgp(kw) = tgp(kw-1) + tstep     
            Du= (tgp(kw)-tres(5)) / tres(5)         ! deviation from  tres(5)
            if( abs(Du) .LT. 1.e-8 ) gpEq(5)= kw 	
         endif	
         if(gpEq(5) .GT. 0) goto 5  
      enddo	     	   

 5    continue
      do kw = gpEq(5)+1, gpEq(5)+50
         if(tgp(kw-1) .LE. tres(6)) then   ! new version
	      tstep = (tres(6) - tres(5))/npa  
            tgp(kw) = tgp(kw-1) + tstep     
            Du= (tgp(kw)-tres(6)) / tres(6)         ! deviation from  tres(6)
            if( abs(Du) .LT. 1.e-8 ) gpEq(6)= kw 	
         endif	
         if(gpEq(6) .GT. 0) goto 6  
      enddo	     	   

 6    continue

      ng = 1       ! "ng" is the order# of t-grid point; here the first t-point
      tf = tgp(ng) ! first t-point of t-grid
    
********************  Main "ti'- tf" loop

 11   continue    ! do next t-step
      ti = tf     ! "tf" of finished (ti-tf] interval becomes "ti" for present interval.
      ng = ng+1
      tf = tgp(ng)

      La= 2  ! BS, instead of La-loop of La=2,3 (Core, Halo)	
	
      CALL SCENARIO(ng)  ! for (ti, tf] interval prescribe this-zone (here, this-La)  D, Te, TiD; Den(XE=1), Den(XE=2), bp, bc, bw.      
c                        Note: the RF computation for any zone requires radii of all zones

***   Print governing params [D,Te,TD,ni, part of Mg in denI (<=1)] on the screen. 
***   I show them at the end of (ti-tf] interval because used for t-integration of POPs from "ti" to "tf". 
         write(*,'(f8.4,  f7.3,    f10.1, f8.2,    e11.3, f7.2 )') 
     +           tf*1.d9, 2*R2*10., Te,  TiD/1000., DenI, paMG*100.  							      

      nX= 0
 15   nX= nX+1 
***                Gaussian FWHM/hvC of spectral lines 
      FWkVcGAU(nX)= 1.66511*sqrt(2.*BolEr*TiD/Imas(nX))/c  ! Doppl FWHM(Ti)/vC = sqrt[ln(2)*8k*Ti/M]/c; see Griem3 p.54 == NRL (25) p.56 == Vain p.248  

      CALL AtKins()  ! Kinetics for this "La" & "nX": find tf-values of POP(kQS) & POPZ(jSS,nX,La); update POPt(k,nX,La) & BEp(k,nX,La)

      if(nX .LT. nXE ) goto 15    ! process next XE

***   Print [R,Te,TD,ni,niAL,niMG,Zbar,ne,u3D] in file MHD_BS.dat. 
***   Relate them to the end of (ti-tf] because used for t-integration up to t=tf. 
      write(32,'(f6.4, f6.3, 2f7.0, f7.2, 2e10.3, f8.3, f8.3, 2e11.4)')   
     +      tf*1.e9, 2.*R2*10, Te, TiCor, TiD/1000., Den, ZC, Dene,      ! [ns] [mm] [keV] [i/cc]
     +      u3D                                                          ! u3D not used in the code, only printed; we use TD. In the code we call it "TiD"

c  Print all-XE POPZ for this "La" & t = tf   			
      do  nX = 1, nXE
        do jSS = FSS(nX), HSS(nX) +1   ! including nucl
           POPZ(jSS,nX,La)= max(POPZ(jSS,nX,La), 1.d-40)
        enddo
      enddo
      write(117,'(f9.4, 10e11.4, f7.3, f9.1, e11.4, f8.2)') tf*1.e9, 
     +  POPZ(10,1,La), POPZ(11,1,La), POPZ(12,1,La), POPZ(13,1,La),     ! "1" is AL:  Be-, Li-, He-, H-like & nucl 
     +  POPZ(14,1,La), POPZ( 9,2,La), POPZ(10,2,La), POPZ(11,2,La),     ! "2" is Mg:  Be-, Li-, He-, H-like & nucl 
     +  POPZ(12,2,La), POPZ(13,2,La), 20.*R2, TeBS,  DenI, TiD/1000.       

      CALL EmiAbso()   ! update this-La EmTot(La,hv,1) & AbTot(La,hv,1). 3rd index inU=1 means "local" (i.e., U==0). Units: W/cc/sr/eV & 1/cm
      CALL EffSpInK13() 
      CALL RTeSpIn()   ! Calculate towards-detector Radiation Flux from BS [W/eV/sr]. 

      do k3= 1, mSpe                ! check all in-FLAG print-times for prints (60+ and (170+ 
         if(ng .eq. gpEq(k3)) then  ! "gpEq(...)" is NUMBER of t-point at which t = tres(...): CALL SUBR "TXscans" 
            CALL TXscans(k3)        ! simulate TREX scans "(60+" and FuFlux "170+" at t-point #k3
         endif 
      enddo

      CALL PowYie()  ! estimate plasma radiation Power(t) and spectral yield (J/eV)
  
      goto 11	 ! Note: the run will be stopped by "if(FrNu .eq. mSpe) STOP 'Finished the Computations.'  
      END ! MAIN



      SUBROUTINE OpenFiles()   ! List spectral lines to be observed in [hvSmo, hvMax] & Print hv-regulated LineList in file #30.
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

      open(40, file= 'Resume_.dat')
      open(41, file= 'Comment.dat')

      open(42,file='PCDrespo8umBe1umCH.inp')  ! response of Be-filtered PCD (absorption of 0.5 mm diamond * transmission of 8mcm Be + 1 mm CH filter) Oct 19, 2011
      read(42,'(a10)') title
      do iv = 1, npBe8
        read(42,*) hv8Be(iv), RespoBe(iv)     ! photon energy & response
      enddo	  		 	  
      close(42)

      open(51,file='Respo5milKapPCD.inp')     ! response of  5 mils kapton filter of hard PCD [1mil = 25.4 mcm; mils= mInch]
      read(51,'(a10)') title
      do iv = 1, np5kap
        read(51,*) hv5kap(iv), Resp5kap(iv)   ! photon energy & response
      enddo	  		 	  
      close(51)

      open(52,file='Respo10miKapPCD.inp')     ! response of 10 mils kapton filter of hard PCD
      read(52,'(a10)') title																																																																			   
      do iv = 1, np10ka
        read(52,*) hv10ka(iv), Resp10ka(iv)   ! photon energy & response
      enddo	  		 	  
      close(52)

      open(53,file='Respo40miKapPCD.inp')     ! response of 40 mils kapton filter of hard PCD
      read(53,'(a10)') title
      do iv = 1, np40ka
        read(53,*) hv40ka(iv), Resp40ka(iv)   ! photon energy & response
      enddo	  		 	  
      close(53)

      open(55,file= 'TX_1520_respo1.inp')      ! TREX response in 1st order of mica reflection
      read(55,'(a10)') title              
      do i = 1, npTX
         read(55,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(1,i)   !  v-grid [keV]
      enddo	  		 	  
      close(55)

      open(56,file= 'TX_1520_respo2.inp')      ! TREX response in 2nd order of mica reflection
      read(56,'(a10)') title              
      do i = 1, npTX
         read(56,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(2,i)   !  v-grid [keV]
      enddo	  		 	  
      close(56)

      open(57,file= 'TX_1520_respo3.inp')      ! TREX response in 3rd order of mica reflection
      read(57,'(a10)') title              
      do i = 1, npTX
         read(57,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(3,i)   !  v-grid [keV]
      enddo	  		 	  
      close(57)

      open(58,file= 'TX_1520_respo4.inp')      ! TREX response in 4th order of mica reflection
      read(58,'(a10)') title              
      do i = 1, npTX
         read(58,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(4,i)   !  v-grid [keV]
      enddo	  		 	  
      close(58)

      open(59,file= 'TX_1520_respo5.inp')      ! TREX response in 5th order of mica reflection
      read(59,'(a10)') title              
      do i = 1, npTX
         read(59,*) hvTX(i), xx1, xx2, xx3, xx4, RespTX(5,i)   !  v-grid [keV]
         hvTX(i)= 1.d3*hvTX(i)                                 !  keV to eV 
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
      open(46, file= 'LineBroBS.dat')

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

      open(371, file= 'MgLyB_shape_transf.dat')   ! Transformations of Jenya's Mg Ly-B shape 

      open (452, file= 'EffSpInK13inPrintFr.dat')                   
      write(452,'(a32)')  'hvEV       EffSpIn     kapR' 	     

      open (100,file='Powers_TW_From_1_BS.dat')                   
      write(100,'(a90)') 'tfNS  FullPowTW    PwBeTW    Pow5mTW    Pow10T
     +W    Pow40TW    RadYieJ     TeKeV      ni'

      open (280, file= 'PCD40k10_func_Te.dat')
      write(280,'(a19)') 'TeKeV  PCDs40to10'
      Return
      END



      SUBROUTINE Intro()  ! Read input files, print list of QSs (true +RY +AI), provide QSs p.q.n., A coefs & initial POPs; print this info in "Comment.dat"  
      use mo1 
      implicit none
      character*1 po1     ! for conversion of QSname2 symbol (1 position) in p.q.n.
      integer char2int    ! symbol-to-integer convertor function
      integer nSS, mth, LL, LU, iSS1, iQS1, iSS2, iQS2, ki, nFi
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

      bra = zero   ! Preliminary. It will be changed to 1 when "Inz.inp" shows inz channel. Used in kinetics & BFFB emi/abso,
      MthdEX = -7  ! "-7" remains for transitions missing in "ExcXE....inp', namely, for impossible non-RY and all involving RY QSs.                    
      kiSS = -66     ! for later check
      nuGS = 0
      kAI1 = 0     ! impo; used for compar (see 11 lines below); there will be "i-1" mistake, if SS ranges down to atom (FSS=1)

c   Read all "QSsXE....inp" files. 
      do nX= 1, nXE
        nFi= 11 + 100*nX        ! file## == 111 & 211 are "QSsXE...inp" in Chin-type bases for AL & MG  
        do k1 = 1, 2
          read(nFi,'(a9)') empty  ! 15 lines of comments 7 titles   
        enddo		 
        nuGS(FSS(nX),nX)= 1     ! GS of First included SS of this XE is given #1 in the QS list of this XE  
        LastAI(nX)= Nnu(nX)     ! preliminary
        do i= FSS(nX), HSS(nX)  ! all SSs of nX, except for NUCL                
          read(nFi,*) nSS, nuQS(i,nX), nuAS(i,nX), PI(i,nX)
          if(nSS.ne.i) then
             write(*,'(a30, 2i3)') 'Inconsistency for XE#, SS=', nX, nSS
             write(*,'(a30, 2i3)') 'i, FSS(nX)=', i, FSS(nX)
		   STOP 'inconsistency #1'
          endif
          if(i.gt.FSS(nX)) nuGS(i,nX)= nuGS(i-1,nX)+ nuQS(i-1,nX)         ! this GS #  in the QS list of XE  
          if(nuAS(i,nX).ne.0) then 
            if(kAI1(i-1,nX).eq.0) kAI1(i,nX)= Nnu(nX) +1                  ! 1st AIQS has # = nucl# +1
            if(kAI1(i-1,nX).gt.0) kAI1(i,nX)= kAI1(i-1,nX) +nuAS(i-1,nX)  ! QS# of the 1st  AIQS of SS=i                 
            kAI2(i,nX)= kAI1(i,nX) + nuAS(i,nX) -1                        ! QS# of the last AIQS of SS=i
            LastAI(nX)= kAI2(i,nX)                                        ! will be used in numbering RY QSs; former "Nw2" 
          endif
        enddo 
        nuGS(HSS(nX)+1, nX)= nuGS(HSS(nX),nX)+ nuQS(HSS(nX),nX)           ! nucl     
        if(nuGS(HSS(nX)+1, nX).ne.Nnu(nX)) STOP 'Inconsistency #2'

  2     read(nFi,*) iSS 
        do k= nuGS(iSS,nX), nuGS(iSS+1,nX)-1                ! all regular QSs of this SS   
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
	     
*  read AI QS names & info & prescribe "kiSS(QS,nX)"
        do j= FSS(nX), HSS(nX)
          if(nuAS(j,nX).lt.1) cycle
          read(nFi,'(a9)') title
          do k = kAI1(j,nX), kAI2(j,nX)  
            read(nFi,'(a5,a5, f3.0, 2f11.3)') QSname1(k,nX),  
     +           QSname2(k,nX), g0(k,nX), E(k,nX) 
            kiSS(k,nX)= j
          enddo 
        enddo
	  		 
        do k = 1, LastAI(nX)          ! all true & AI QS# of this XE, no RYs yet
          if(k.eq.Nnu(nX)) cycle      ! skip nucl
          po1 = QSname2(k,nX)(2:2)    ! (2:2) means "positions from 2 until 2"; it must be position #7, which contains p.q.n. of outer electron 
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
      enddo   ! relates to nX-loop of reading/processing "QSsXE.inp"

c  Read excitation cross sections & f's from all 'ExcXE....inp' files.  Note: last line of "ExcXE...inp" MUST corresp to Nnu-2 --> Nnu-1
      do nX= 1, nXE
        nFi= 12+ 100*nX             ! "ExcXE.inp" file## == 112, 212, 312, 412 
        read(nFi,'(a9)') title
        read(nFi,'(a9)') empty  
  6     read(nFi,*) iSS, LL, LU, mth, Axw, Bxw, Cxw, Dxw, Exw, Fxw, fw 
        if(LL.lt.nFAI) kL= nuGS(iSS,nX)-1+LL       ! order# in XE total QS list for regular QS           
        if(LU.lt.nFAI) kU= nuGS(iSS,nX)-1+LU
        if(LL.ge.nFAI) kL= kAI1(iSS,nX) + LL-nFAI  ! order# in XE total QS list for AI QS
        if(LU.ge.nFAI) kU= kAI1(iSS,nX) + LU-nFAI 
        if(abs(Fxw).gt.1.d-30) STOP 'OPEN "Fx" array'

        DE= E(kU,nX) - E(kL,nX) 
		write(*,'(a20, i9, i9, i9)') '->>>>>', iSS,LL, LU
        if(DE .le. zero) STOP 'Excitation down in reading Exc...inp'

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
        if(fw.GT.0. .and. mth.eq.0) fVR(kL,kU,nX) = fw               !  fw > 0 is stored for Van-Regemorter formula only; f & A== 0
        if(fw.GT.0. .and. mth.ne.0) STOP 'found f>0 for non-VR method'
        A(kU,kL,nX)= 4.3450d7* flu(kL,kU,nX) *g0(kL,nX)
     +               *(E(kU,nX)-E(kL,nX))**2 /g0(kU,nX)

        if(iSS.eq.HSS(nX) .and. kL.eq.Nnu(nX)-2 .and. 
     +                          kU.eq.Nnu(nX)-1) goto 7                  ! It must be the last line of "ExcXE.inp"
        if(iSS.eq.HSS(nX) .and. LL.eq.13 .and. 
     +                          LU.eq.16) goto 7                  ! It must be the last line of "ExcXE.inp"
        if(mth.eq.-5 .or. mth.eq.0  .or. mth.eq.5 .or. mth.eq.11) goto 6 ! "-5" is "5 of low accyracy"; Lenya

        write(*,'(a65,5i5)') 'Excit CrosSec fit-method is unknown for XE, 
     +  iSS, LL, LU, mth=', nX, iSS, LL, LU, mth
        STOP
  7     close(nFi)
      enddo        ! for XE-loop, which reads "ExcXE.inp" files 

      read(13,*) hvMin  ! minimal photon energy hv(eV) on the v-grid
      read(13,*) hvSmo  ! edge of [hvMin,hvSmo] domain, where lines have life-time FWHM > 1 eV, thus merge & do not need fine resolution
      read(13,*) hvMiF  ! First hv-point of superFine grid
      read(13,*) hvMaF  ! Last  hv-point of superFine grid
      read(13,*) hvMax

************************  hv-grid
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

c  Kill weak lines and lines on the edges of v-grid  :
      do nX= 1, nXE
      do k  = 1, NST(nX)  ! L-state: all QSs 
      do k2 = 2, NST(nX)  ! U-state: all QSs 
         if(flu(k, k2,nX) .LT. fluMin .or.     ! if  f < fluMin : kill this line for shortening v-grid & computation time
     +        A(k2,k ,nX) .LT. AuLMin .or.     ! if  A < AulMin : kill this line for .......... 
     +       (E(k2,nX)-E(k,nX)).LT. hvSmo)     ! if close to soft edge of v-grid (wings cut, broadening,...) in any case EmiFF > EmiBB on soft edge    
     +        A(k2, k, nX) = zero              ! it will exclude the line from the LineList.             
      enddo                                                                                                 	
      enddo
      enddo

c  Read AI Probabilities from 'AIwXE....inp' file
      do nX= 1, nXE
        nFi= 14+ 100*nX        ! "AIwXE....inp"  files are ## == 114, 214, 314, 414 
        read(nFi,'(a9)') title
  8     read(nFi,*) iSS1, iQS1, iSS2, iQS2, AIw, trEn  ! the energy "trEn" is for consistency control only 
        if(iQS1.lt.nFAI) STOP 'non-AI initial state in "AIw...inp"'   
        if(iQS2.ge.nFAI) STOP 'AI final state in "AIw...inp"'     
        if(iSS1.eq.111) goto 402                                    ! "111" shows stop-reading line: file end.  MY contribution.

        ki= kAI1(iSS1,nX)+ iQS1-nFAI  ! order# in XE total QS list for AI initial QS 
        kf= nuGS(iSS2,nX)-1 +iQS2     ! order# in XE total QS list for regular final QS
Consistancy control 1: 
        if(kiSS(kf,nX) .ne. kiSS(ki,nX)+1) then
	     write(*,*) 'XE, ki, kf =', nX, ki, kf 
           STOP 'have error in "AIaXE...inp" level numbers'
        endif
Consistancy control 2: AI transition energy DE == E(qAI) - [PIR + E(fin)], i.e. E(i) - E(f), where both E taken 
c                      relative to common "0", which is GS of "2ex ion".  Here we check consistency of DataBases: 
c                      compare "trEn" from "AIwXE...inp"  to  E(qAI)-[PI+E(fin)] from "QSsXE....inp".
        if(abs(trEn-(E(ki,nX)-PI(iSS1,nX)-E(kf,nX))) .gt. 0.002) then
           write(*,'(a25)') 'Inconsistency in AI transition energy:'
           write(41,'(i2, 6i4, 6e12.6)') nX, ki, iSS1, iQS1, 
     +       kf, iSS2, iQS2, trEn, E(ki,nX)-PI(iSS1,nX)-E(kf,nX), 
     +      E(ki,nX), PI(iSS1,nX), E(kf,nX)  
	    STOP 
        endif
        WAiz(ki,kf,nX)= AIw
        goto 8
 402    close(nFi)
      enddo       ! nX loop for AI

****** Read the ionization cross-secs coefs from "InzXE....inp" & assign 1/0 to "bra(i,f,XE)" based on Yes/No ioniz channel in "InzXE...inp" 
      do nX= 1, nXE
        nFi= 13+ 100*nX         ! "InzXE....inp"  files are ## == 113, 213, 313, 413 
        read(nFi,'(a9)') comme
        read(nFi,'(a9)') title
  9     read(nFi,*) iSS1, iQS1, iSS2, iQS2, Axw, Bxw, Cxw, 
     +              Dxw, mth, Exw, Fxw, Gxw, Hxw, thre     ! "thre" is ioniz threshold
        if(iSS1.eq.111) goto 405                           ! "111" is a sign of stop-reading (technical) line that I added
        if(iQS1.lt.nFAI) ki= nuGS(iSS1,nX)-1 +iQS1         ! order# in XE total QS list for regular QS 
        if(iQS1.ge.nFAI) ki= kAI1(iSS1,nX)   +iQS1-nFAI
        if(iQS2.lt.nFAI) kf= nuGS(iSS2,nX)-1 +iQS2         ! order# in XE total QS list for regular QS 
        if(iQS2.ge.nFAI) kf= kAI1(iSS2,nX)   +iQS2-nFAI
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
        bra(ki,kf,nX)= one  ! "one" means yes single-e ionization channel between ki & kf. With FAC-bases "bra" is either 1 or 0
        goto 9
 405    close(nFi)
      enddo        ! nX loop for Ioniz cross-sec

c  Read 'Flag.inp'
      do nX= 1, nXE
        read(13,*) kQS      ! QS# for initial-POP-load
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
      read(13,*) PrFr    ! = 1,2,...6 is "tres(#)" for printing "PCDs_Resp_i_Pow_at_PrFr.dat", 'LineBroBS.dat', 'EmiAbsoDetails.dat', ...
  
      read(13,'(a9)') empty
      read(13,'(a9)') empty
      read(13,*) strt, Dstrt, TeStrt, TDstrt, nStrt, peMGst   ! initial values [cm, eV, keV, i/cc   %]
	do i = 1, mSpe
         read(13,*) tres(i), Dfr(i), TeFr(i), TDfr(i), nFr(i), peMG(i)  ! s, cm, eV, keV, i/cc   perc
      enddo

      read(13,'(a9)') empty
      read(13,*) PoLeNu(1,1), PoLeNu(1,2), PoLeNu(1,3), PoLeNu(1,4),
     +           PoLeNu(1,5), PoLeNu(1,6)             
      read(13,*) PoLeNu(2,1), PoLeNu(2,2), PoLeNu(2,3), PoLeNu(2,4),
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

      write(46,'(a63, i1, a70)') 'Info on the Line Broadening (FWHM/hvC)
     + for BS at Fr', PrFr, 'time.  We omitted lines with PopU < 1.d-8 a
     +nd lines with hvC < hvSmo.'
      write(46,'(/a137)') 'hvCeV   XE  SpS  Lambda[A]      UPPER        
     +  LOWER      GauTD   Jstark    Natur   Lorentz   Voigt   WoutUp   
     +WoutLo    PopL     PopU'
      Return
      END	  ! of INTRO subr


 
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
         CASE ('l')	  ! in 4l AIQSs of [He], say neon
           char2int = 0
         CASE DEFAULT
           WRITE (*,*) symbol, ' not convertible into integer'
           STOP 'STOPped in function char2int'
        END SELECT
      END


      SUBROUTINE LineList()   ! List spectral lines to be seen in [hvSmo, hvMax] & Print hv-regulated LineList in file #30.
	use mo1
	implicit none
	integer lnew, iw, lw, lmin, nXmin ! kSP   ! w-variables
	real(8) hvCmin,	Alamda
	open (30, file='LineList.dat')
      write(30,'(/a30, e7.2, a7,f5.4, a14/)') 
     +          	'Lines with A >', AulMin, ', flu >', fluMin, 
     +            ',  hvC > hvSmo'

      write(30,'(a81/)') 'XE  SpS     hvC(eV)   Lamda(A)     A(Hz)      
     +Upper Level     Lower Level       f'

      linM4= 0
      do nX= 1, nXE         ! counting lines between all QSs of each XE.
        lin=0               ! # of Spectral Line found in [hvSmo, hvmax]
        do k  = 1, NST(nX)  ! Lower state 
        do k2 = 2, NST(nX)  ! Upper state
          if(A(k2,k,nX) .LT. 1.) goto 1   ! Weak lines & lines at hv < hvSmo got A=0 in "Intro" subr  
          lin = lin +1
          nLoh(lin,nX)= k                 ! Lower level# prior to regulation of the line numbers according to their hvC
          nUph(lin,nX)= k2                ! Upper
          hvCh(lin,nX)= E(k2,nX)- E(k,nX) ! Spectral line center [eV]
  1       continue
        enddo
        enddo
        linM(nX)= lin            ! number of lines in the domain
        linM4= linM4 +linM(nX)   ! same for all XEs
      enddo

      if(linM4.ge.MNLe) then
        write(*,'(a20,i6, a4,i6)') 'Increase MNLe=', MNLe, 'to', linM4 
        STOP
      endif																															    

c  Regulation according to "hvC" (thru all XE)
      do lnew= 1, linM4    ! It will be a Line# after regulation along hv (in the common line list for all XEs)
        hvCmin= 1.d7       ! arbitrary value before search
        do nX= 1, nXE
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
        if(hvC(iw) .lt. hvSmo) STOP '  found Line at hv < hvSmo'	
        if(hvC(iw) .gt. hvmax) STOP '  found Line at hv > hvMax'
        Alamda= eVA/hvC(iw)          ! A
        write(30,'(i2, i4, 2f12.3, e11.3, i7, a1, a4,a4, i7, a1, a4, a4, 
     +             e12.3)')  nX3(iw), kiSS(nUp(iw),nX3(iw)),  
     +       hvC(iw), Alamda, A(nUp(iw),nLo(iw),nX3(iw)), nUp(iw),'=', 
     +       QSname1(nUp(iw),nX3(iw)),QSname2(nUp(iw),nX3(iw)), nLo(iw), 
     +       '=', QSname1(nLo(iw),nX3(iw)), QSname2(nLo(iw),nX3(iw)),
     +       flu(nLo(iw),nUp(iw),nX3(iw))      
      enddo
      linM4= linM4       ! total lines in [hvSmo, hvmax] interval
	close(30)

	do lin= 1, linM4
	  FWvoi(lin)= hvC(lin)*1.d-4  ! LineWidth info For 1st CALL of 'Grid"
      enddo
      Return
      END	 ! of 'LineList' subr



      SUBROUTINE EmiAbso() ! this-La emissivity "EmTot(La,hv,1)" [W/cc/sr/eV] & absorptivity "AbTot(La,hv,1)" [1/cm] due to all XEs
	use mo1              ! NOTE: in "EmTot" & "AbTot" the 3rd index =1  means "for uLOS = 0" i.e., "prior to the hydro shift"  
	implicit none
	integer lw 
      real(8) EmiLFul(MNLe), AbsoAm(MNLe),  ! Amplitude of bound-bound (BB) emissivity & absorptivity
     +         AlfaBB(MNLe),                ! (nU/gU)/(nL/gL) 
     +        absorBB(nvM),  emisBB(nvM),     
     +        absorFF(nvM),  emisFF(nvM), pBB(nvM), ArPBB(MNLe),   
     +        DiZ2, FrE,  dev, FWevGau, DEw, HaLor, EED,  
     +        SigRR, Vel, hvJ, contrib,  alfaFB, Voigt2,      ! Voigt2 is Sawa's connector to JQSRT Viogt; Earlier used "VoigtJS" that is Jenya's Voigt function but is was WRONG 
     +        SigPhI, FWevNatu, HoltSum, StJen, pqnUp         ! Gauss  ! DynamW, QuSfact, JenR, QuaStatW, n2dif, 

      if(La .ne. 2) PAUSE 'La =/= 2 in subr EmiAbso'

      HoltSum= zero
      do n7= 1, nXE
        HoltSum= HoltSum + Den(n7)*ZC(n7)**1.5
      enddo
      HoltSum= HoltSum**0.6667  ! This type of averagimg is needed for Holtsmark F0 of Chem mixture [see E.St.); F0 ~ Z*Den^2/3  (4.56, Griem3)      		            
      do lw= 1, linM4           ! line# runs thru all XE, being ordered with hvC(iw)
        EmiLFul(lw)= zero   ! Weak lines & out-of-hv-interval lines are already excluded
         AbsoAm(lw)= zero
         AlfaBB(lw)= zero
        FWevLor(lw)= zero
        if(hvC(lw) .lt. hvSmo)     STOP '  Line with hvC < hvSmo'
        if(hvC(lw) .gt. hvMax*0.9) STOP '  Line with hvC > hvMax'
        kU = nUp(lw)
        kL = nLo(lw)
        nX = nX3(lw)	                                   ! nX3(iw) is XE# for this line 
        if(kU.LT.Nnu(nX) .AND. BE(kU).LT. 1.d-3) goto 1  ! skip line from non-AI dead QS
        if(kL.LT.Nnu(nX) .AND. BE(kL).LT. 1.d-3) goto 1  ! skip line onto non-AI dead QS

        EmiLFul(lw)= BolJ*hvC(lw)*A(kU,kL,nX)*           ! In-Full-Shape Emissivity, W/cc/sr
     +                    POPt(kU,nX,La)*Den(nX)/FoPi   				 
        if(POPt(kL,nX,La).gt.1.d-15) AlfaBB(lw)= 
     +                               (POPt(kU,nX,La)/g0(kU,nX))/
     +                               (POPt(kL,nX,La)/g0(kL,nX))
        AbsoAm(lw)= 1.098d-16* flu(kL,kU,nX) *POPt(kL,nX,La)*Den(nX) ! Absorptivity (16) except *P(hv); i.e. [eV/cm], see (18)
     +                       * (one-AlfaBB(lw))
  1     continue

        FWevNatu= h* Wout(kU,nX,La)/ToPi  ! eV; Smallest possible Lorentzian FWHM, 
c                                           used for most Lines because "StJen" is defined only for hvC > 1.2 keV.
	    
        StJen = 0.     ! helps to find "skipped" transitions; they will show "0" in "write(47,"
c  My approximate expression for Lyman lines and similar lines of He-like ions, Based on "WIS-formulary"
        if(hvC(lw).gt. 1200.) then    ! [eV], K-shell lines, including sats  
          pqnUp= nprin(kU,nX)         ! Line is due to transition of OuterMost e.
c                                       although some sats are due to transi of sub-OuterMost, say 2p3d --> 1s3d 
          if(kU.gt.Nnu(nX)) pqnUp=2   ! Satellite; in our DaBa sats are only to Alpha-lines; 
c                                       Scale them like Alpha lines because E.St. has nothing to say on Stark of sats. 
c                                                             Scaling based on WIS Formulary
          if(pqnUp.eq.2 .and. nX.eq.1) StJen= 0.144*          ! [eV] AL Ly-A & He-A using AL LyA scaling in Formulary of Nov 1, 2022
     +              (Dene/1.3d22)**0.405 * (Te/1.d3)**0.463   ! assuming pure AL, Ti= 2Te, Z=13 for Te= (1- 1.4) keV, ni = (1-4}d21 i/cc

          if(pqnUp.eq.2 .and. nX.eq.2) StJen= 0.199*          ! [eV] MgLyA & MgHeA based on scaling of Mg Ly-A in Formulary of Nov 4, 2022 assuming
     +              (Dene/1.3d22)**0.398 * (Te/1.d3)**0.454   ! 5% Mg in Al plasma of Z=13, Ti= 2Te, Te= (1-1.4) keV, ni = (1-3}d21 i/cc

          if(pqnUp.eq.3 .and. nX.eq.1) StJen= 7.0 *           ! Al Ly-b (at hv= 2.0483 keV) is out of TX spectrum but adds to radiative loss.
     +                   (Dene/3.d22)**0.6 *(Te/1.d3)**0.2    ! Formulary; Nov 4, 2022, pure AL, Ti=2Te, Z=13, Te= (1- 1.4) keV, ni= (1-3}d21 i/cc 

          if(pqnUp.eq.3 .and. nX.eq.2) StJen= 5.08 *          ! Mg Ly-b (hv= 1.7447 keV in FAC DaBa used here). Formulary of Nov 4, 2022 
c                                                               assuming 5% Mg in Al plasma of Z=13, Ti= 2Te, Te= (1-1.4) keV, ni= (1-3}d21 i/cc
     +              (Dene/1.3d22)**0.566 *(Te/1.d3)**0.324    ! I don't use this ne-scaling in transformation of Jenya's 4 shapes of Mg Ly-B,
c                                                               there i use ne-scaling derived from Jenya's FWHMs, no Formulary; see "ShapeJV". 
c                                                               BUT present Te-scaling is YES used there, because all J-shapes are for Te= 1300 eV) 
c                                                               "StGen" is used in computation of POPs, emissivity, absorptivity, thus for.
c                                                               "SpIn" & RadFlux. Jenya's 4 shapes of MgLyB used only for its FWHM in eqs (5) and (15).  
          if(pqnUp.eq.4) StJen= 9.3 *(AtMass(1)/AtMass(nX))*  ! "J-WIS-formulary" gave 9.28 for AL HeG 
     +                   (Dene/1.d22)**0.6 *(Te/1.d3)**0.2  
        endif

        FWevGau= hvC(lw)* FWkVcGAU(nX)     ! eV; Gauss FWHM of line #'lw' in plasma with TiD(La)
        fwTiKvC = FWkVcGAU(nX)             ! Gau FW(TiD)/hvC; only for this print; "FWkVcGAU" is determ via TiD(La) & Imas(nX) 
        FWevLor(lw)= max(FWevNatu, StJen)  ! Baranger' "natural" limit of Lor width
c                                          IMPO: "StJen" is defined for hv > 1200 eV only, 
c                          "FWevNatu" =0 for dead levels because their "Wout(kU,nX,La)"==0
c                             thus many lines are formally present but their Upper level is dead and FWevLor(lw)=0
c                                I excluded them from print (46, by "PopU > 10^-8". 
        HaLor= FWevLor(lw)/2.							
        FWvoi(lw)= HaLor + sqrt(HaLor**2 +FWevGau**2)  ! from JQSRT on Voigt subr
        fwTUKvC = 0.                                   ! not defined in this version of the code because we use TiD(La) for (Ti+hydr) motion 

        if(kL.eq.93 .and. kU.eq.96 .and. nX.eq.2) then ! Mg Ly-A 3/2 ; yes printed
          MgLyA_FW_Gau  = FWevGau 	  
          MgLyA_FW_Natu = FWevNatu	  
          MgLyA_FW_StJe = StJen	  
          MgLyA_FW_Lor  = FWevLor(lw) 
	  	MgLyA_FW_Voi  = FWvoi(lw)
        endif 	   

        if(ng .eq. gpEq(PrFr) .and. POPt(kU,nX,La).gt. 1.d-8	          ! "ng" is the number of "tf" point in t-grid (thus, the number of "ti-tf" interval)
     +                        .and. hvC(lw) .GT. hvSmo)                 ! "gpEq(j)" is NUMBER of t-point at which tf = tres(j)
     +     write(46,'(f9.3, 2i4, f11.4, i6,a1,a4,a4, i6,a1,a4,a4,9e9.3) ! transitions betw non-Ry states 
     +              ') hvC(lw), nX, kiSS(kU,nX), eVA/hvC(lw),    	
     +                 kU, '=', QSname1(kU,nX), QSname2(kU,nX),  
     +                 kL, '=', QSname1(kL,nX), QSname2(kL,nX), fwTiKvC,  	
     +            StJen/hvC(lw), FWevNatu/hvC(lw), FWevLor(lw)/hvC(lw),        
     +               FWvoi(lw)/hvC(lw), Wout(kU,nX,La), Wout(kL,nX,La),  ! 1/s
     +                            POPt(kL,nX,La), POPt(kU,nX,La)           
      enddo ! lw 

c  BB  EMISSION & ABSORPTION due to spectral lines of ALL XE;
c . 						   Weak lines and out-of-hv-interval lines are excluded from LineList
c                            Lines from dead QSs and onto dead QSs came with EmiLFul(lw)=0, AbsoAm(lw)=0
       emisBB= zero    
      absorBB= zero
      do lw= 1, linM4  ! 
        FWevGau = hvC(lw)* FWkVcGAU(nX3(lw))  ! Gaussian FWHM [eV] due to TiD
        Wing    = hvC(lw)/2.                  ! Line Wing Length [eV]
        ArPBB(lw) = zero      ! Area under line shape pBB(v), was =1 until wings cut 
        do iv= 1, nvM
           dev  = abs(hvV(iv)-hvC(lw)) 
           pBB(iv) = zero                    ! this lw
           if(hvV(iv).GT. hvC(lw)-Wing .and. 
     +        hvV(iv).LT. hvC(lw)+Wing      ) then  
              pBB(iv) = Voigt2(FWevLor(lw), FWevGau, hvC(lw), dev)  ! JQSRT (Sawa) Voigt. It is slower (5x ?? than Jenya's VoigtJS) but correct to 1% 
           endif                                             
           dhv = hvV(iv)-hvV(iv-1)
           if(iv.GT.1) ArPBB(lw)= ArPBB(lw) +     ! Area under shape < 1 because line wings cut thus part of photons lost. 
     +       	         dhv*(pBB(iv)+pBB(iv-1))/2. ! To return them, cut-wings shape will be re-norm to Integ[pBB(v)dv] = 1
        enddo   ! iv

        if(ArPBB(lw) .LT. 0.1) then
           write(*,'(/a36, i6, f10.3, 9e10.2)') 
     +       'lw, hvC, FWevGau, ArPBB(lw) =', 
     +        lw, hvC(lw), FWevGau, ArPBB(lw)
	     STOP '  ArPBB(lw) < 0.01)' 
        endif

        do iv= 1, nvM                             
           emisBB(iv)= emisBB(iv)+ EmiLFul(lw) *pBB(iv) ! BB emissivity [W/cc/sr/eV] DUE TO ALL lines of this XE, at hveV
     +                                       /ArPBB(lw) ! return photons from cut wings  
          absorBB(iv)= absorBB(iv)+ AbsoAm(lw) *pBB(iv) ! [eV/cm]*[1/eV] = 1/cm  (16) DUE TO ALL LINES
     +                                       /ArPBB(lw) ! return photons from cut wings  
        enddo    
      enddo     ! lw

c  FF  EMISSION & ABSORPTION due to scattering of ALL free electrons on ions of ALL XE. 
      do iv= 1, nvM
         hveV = hvV(iv)
         DiZ2= DenI*ZC2                                           ! all XE, see definition of ZC2
         emisFF(iv) = 1.21d-33*DiZ2*Dene*exp(-hveV/Te)/sqrt(Te)   ! [W/cc/sr/eV], (61); MAXWELLIAN EED
         absorFF(iv)= 2.4006d-37* Dene*DiZ2 *(1.-exp(-hveV/Te))/  ! (74), including induced-emi correction in Maxw case. 
     +	                                    sqrt(Te)/hveV**3    ! Notice 1% correction in 2.4006 to fit 5040.36 W in Planck.    
      enddo

c  FB EMISSION due to RR on ions of both       Notations: Recomb (SS=j+1,k) + e  --> (j, kf) + hv
      if(ti.LT.strt+ 2*tstep) goto 90        ! Skip FBBF on 1st t-step because BE not computed
      do iv= 1, nvM
        emisFB(iv)= zero                 
        hveV = hvV(iv)
        do nX= 1, nXE  
          do k = 1, NST(nX)   
            do kf= 1, NST(nX)
               if(bra(kf,k,nX).LT. 0.5)  goto 65  ! no Ionz/Recomb link
               j = KiSS(kf,nX)
               if(k .LT.Nnu(nX) .AND. BE(k ).LT. 1.d-3) goto 65  ! dead non-AI QS; Note: AIQSs are treated as "always alive" 
               if(kf.LT.Nnu(nX) .AND. BE(kf).LT. 1.d-3) goto 65  ! deak non-AI QS        although given BE= -13 in Intro

               BEk= Eth(kf,k,nX) - DPI(j,nX)   ! min energy for ionization (j,kf) --> (j+1,k)
               if(kf.GT.Nnu(nX) .AND.          ! AIQS
     +            BEk .LT. zero)   BEk= 0.01   ! because of big DPI, this kf-->k inz of kf=AI  
c                                              does not require E, but I give formal BEk= 0.01 eV to avoid /0. 
c                                                  Note: "BEk < 0" never happens to regular kf because big DPI cuts it. 
               if(hveV .LT. BEk +1.d-7)  goto 65   ! such small'hv' cannot be emitted in this k\kf recomb; 1.d-7 excludes FrE=0
               FrE= hveV - BEk                     ! energy of free electron before capture:  hv= FrE +BEk
               Vel= 5.930968d7*sqrt(FrE)           ! Velocity of free electr = sq(2k*E/m); cm/s  
               hvJ= BolJ*hveV

               ReverseKKF = 2.                          ! to use function "SigRR(FrE)" reverse k & kf because "SigRR" uses notations of "SigPhi" 
               contrib= Dene*hvJ*Den(nX)*POPt(k,nX,La)* ! one k-->kf channel to (37) sum
     +                  Vel*EED(FrE)*SigRR(FrE)/FoPi
               emisFB(iv)= emisFB(iv) + contrib         ! Sum (37), W/cc/sr/eV
 65           continue                
            enddo      ! kf                            
          enddo        ! k 
        enddo          ! nX
      enddo            ! iv

c  BF ABSORPTION corrected for induced inverse process. Expressions (50) & (49) in BlackFold.
c                              Notations: (j,k) + hv -->(j+1,kf) + e , like in function "SigPhi" to be used
      do iv= 1, nvM
        absorBF(iv)= zero		 
        hveV = hvV(iv)
        do nX= 1, nXE  
          do k = 1, NST(nX)   
            do kf= 1, NST(nX)
               if(bra(k,kf,nX).LT. 0.5)                 goto 83  ! no Ionz/Recomb link
               if(k .LT.Nnu(nX) .AND. BE(k ).LT. 1.d-3) goto 83  ! dead non-AI QS; Note: AIQSs are treated as "always alive" 
               if(kf.LT.Nnu(nX) .AND. BE(kf).LT. 1.d-3) goto 83  ! deak non-AI QS        although given BE= -13 in Intro

               j = KiSS(k ,nX)
               BEk= Eth(k,kf,nX) - DPI(j,nX)  ! min energy for (j,kf) to (j+1,k) ionization is softest-photon hv emitted in RR; this photon is due to free el with E=0.
               if(k.GT.Nnu(nX) .AND.          ! AIQS
     +            BEk .LT. zero)    BEk= 0.1  ! because of big DPI, k-->kf inz of k=AI  
c                                             does not require E, but I give formal BEk= 0.1 eV to avoid 0. 
c                                                  Note: "BEk < 0" never happens for regular k because big DPI cuts it. 
               if(hveV .LT. BEk +1.d-7)  goto 83   ! such small'hv' cannot photo-ionize k to kf; 1.d-7 excludes FrE=0
               FrE= hveV - BEk                     ! energy of free electron before capture:  hv= FrE +BEk
               Vel= 5.930968d7*sqrt(FrE)           ! Velocity of free electr = sq(2k*E/m); cm/s  

               Ww= 1.46796094d-22*Dene*EED(FrE)/sqrt(FrE)  ! We write now "1.468" instead of "2.936../two" because already proven that our guess on the error in (49) is correct, see comment in true BlackFold
               popgrat= POPt(kf,nX,La)*g0(k ,nX)/
     +                  POPt(k, nX,La)/g0(kf,nX)
               alfaFB= Ww*popgrat
               contrib= Den(nX)*POPt(k,nX,La)*SigPhI(hveV)*(one-alfaFB)  ! (49)
               absorBF(iv)= absorBF(iv)+ contrib                         ! (50)

 83            continue    
            enddo       ! kf -loop
          enddo         ! k  -loop   
        enddo	          ! nX-loop
      enddo             ! iv-loop
 90   continue

c      "emisFB(iv)" & "absorBF(iv)" computed above correspond to ion-sphere PIR(jSS,nX) of individual 
c      photo-ionized (or FB-capturing) ion. Diffference in e/i environment of individual ions causes 
c      uncertainty in their individual PIR, thus in FBBF edges.  Scale of the uncertainty is DPI(jSS,nX).
c      but because of many e,i neighbours, variation of DPI is small, say PIR(jSS,nX)/2, 
c      therefore the uncertainty in FBBF edges (DEw) taken = 

      DEw = DPI(12,1)/2.  ! nX= 1 is AL; jSS= 12 is He-like 

      emFBcon = zero
      CALL GauConvo( emisFB, emFBcon, DEw, 2)  ! Convo Mode (CoMo)= 2 means convo with Gau of DE(eV) 

      abBFcon = zero
      CALL GauConvo(absorBF, abBFcon, DEw, 2)  

      do iv= 1, nvM
         emTot(La,iv,1) =  emisBB(iv)+  emisFF(iv)+ emFBcon(iv)  ! W/cc/sr/eV;  NOTE: in em&ab, 3rd index =1 means "local", i.e.  
         abTot(La,iv,1) = absorBB(iv)+ absorFF(iv)+ abBFcon(iv)  ! 1/cm;              NO v-shift caused by plasma flow velocity
      enddo   ! iv-loop

      if(ng .eq. gpEq(PrFr)) then    ! "ng" in  the NUMBER of "tf"-point in t-grid
         open(81, file= 'EmiAbsoDetails.dat')
         do iv= 1, nvM
            if(iv.eq.1) write(81,'(a124)')  
     +          'hvEV     iv     emisBB     emisFF     emisFB    emFBcon
     +     emTot     absorBB    absorFF    absorBF    abBFcon     abTot'  
              
            write(81, '(f9.3, i7, 19e11.4)')  hvV(iv),  iv,       
     +         emisBB(iv), emisFF(iv), emisFB(iv), emFBcon(iv), 
     +         emTot(2,iv,1),	absorBB(iv), absorFF(iv),
     +         absorBF(iv), abBFcon(iv), abTot(2,iv,1) 
         enddo 
         close(81)
      endif 

      Return
      END     ! of 'EmiAbso' subr



      SUBROUTINE EffSpInK13()  ! 4pi-average RF "SpInEff(hv)" [W/eV/sr/cm2] for  Win, Wab, WphI & WiRR
      use mo1                  ! Here we use 119-point fit to spherical K13 function K=K(kD/2) 
      implicit none
      real(8) K13, So, tau     ! SpInB 

      if (La.ne.2) STOP 'Asked SpInEff for La =/= 2' 

      do iv= 1, nvM
c       if(La.ne.2) SpInEff(La,iv)= 0. 
        if(La.eq.2) then                        ! La=2 is spherical BS
           tau= abTot(2,iv,1)* R2               ! can be < 0 in case of POPs inversion, see 3 lines below   
           if(abs(abTot(2,iv,1)) .gt. 1.d-20)   ! exclude /0. 
     +        So= emTot(2,iv,1)/abTot(2,iv,1)   ! [W/cc/sr/eV] / [1/cm].  If"abTot" < 0 due to POPs inversion (alfa > 0), "So" is also < 0 but
c                                                         ! "SpInEff" remains > 0 because tau < 0 made (1-dexp(-tau) < 0
           SpInEff(2,iv)= So*(one- dexp(-tau))* K13(tau)  ! spherical K13; For tau < 0 it takes K13= Ksp(1)= 0.75
           if(ng .eq. gpEq(PrFr))                         ! "ng" is the number of "tf" point (thus, the number of "ti-tf" interval)
     +         write(452,'(e13.7, 7e11.4)') hvV(iv),      ! "gpEq(j)" is NUMBER of t-point at which tf = tres(j)
     +                           SpInEff(2,iv)/1.d6, tau  ! MW/cm2/sr/eV 
        endif
      enddo
      Return		  
      END     ! 'EffSpIn' subr



      real(8) function K13(kR)   !  Linear interpolation for spherical K13
      use mo1
      implicit none
      real(8) kR
      K13= Ksp(1)      ! remains for kR <= kRp(1)  
      do i= 2, K13po
        if(kR.gt.kRp(i-1). and. kR.le.kRp(i)) then
	   K13= Ksp(i-1)+(Ksp(i)-Ksp(i-1))*(kR-kRp(i-1))/(kRp(i)-kRp(i-1))
         goto 1
        endif		      	  
      enddo
      if(kR.gt.kRp(K13po)) K13= Ksp(K13po) 
  1   continue
      end          



      SUBROUTINE RTeSpIn() ! Analytical solu of Rad.Trans.Eq. along 10 parallel LOSs gives "RadFlux(iv)" [W/eV/sr].
c                            Present subr works at each t-step because needed for POWER(t) via "RadFlux"  
      use mo1 
      implicit none															   
      real(8) ArK(10), Rout(10), rw(10), pathBS(10),  ! outer radius of k-th radial zone [k= 1...10]
     +                 ArBS, So2, exw   

      ArBS= pin*R2**2   ! Area of cross-section seen to detectors
      Rout(1) = R2/10.           ! Outer radius of first radial zone
       ArK(1) = pin*Rout(1)**2   ! Area of first radial zone 
        rw(1) = Rout(1)/2        ! middle of first radial zone, to be used as "y" of a LOS        
      do k = 2, 10
         Rout(k) = (R2/10.)*k	                       ! Outer radius of k-th radial zone
          ArK(k) = pin* (Rout(k)**2 - Rout(k-1)**2)  ! Area of k-th radial zone 
           rw(k) = (Rout(k) + Rout(k-1))/2.          ! middle of k-th radial zone, to be used as "y" of a LOS
      enddo 
      do k = 1, 10
         pathBS(k)= 2.*sqrt(R2**2 - rw(k)**2)  ! thru-BS path of k-th LOS; 
      enddo 

      do iv= 1, nvM
         if(abs(abTot(2,iv,1)) .gt. 1.d-20)    ! exclude "/0." in next line; La=2 is BS, 3rd index =1 means "no Doppler shift" 
     +      So2 = emTot(2,iv,1)/abTot(2,iv,1)  ! in case of POPs inversion: "abTot" can be < 0  (alfa > 1) but 
c                                                "SpOut" remains > 0 because [1-dexp...] is also negative
         RadFlux(iv) = 0.
         do k = 1, 10 
            exw = dexp( -abTot(2,iv,1)* pathBS(k) )   
            RadFlux(iv) = RadFlux(iv) + ArK(k)*So2*(1.-exw)   ! [W/eV/sr] from BS tovards a detector
         enddo 
         SpOut1(iv) = RadFlux(iv)/ArBS  ! More convenient are [W/eV/sr/cm2] units, therefore relate "RadFlux" to towards-detector area of BS, 
      enddo   ! iv
      Return
      END     ! of subr 'RTeSpIn"



      SUBROUTINE TXscans(FrNu) ! Simulate a scan of TX using RadFlux [W/eV/sr] from entier round area of BS. No Doppler shift
      use mo1 
      implicit none
      integer i2, FrNu  
      real(8) Conv1(nvM), Conv1bb(nvM), MCPor(5,nvM), MCP(nvM), 
     +        ScaF, PeakMgLyA, HalfH, ContiA 

c  Print "RadFlux" at "FrNu" time [in file (60+ that is "FuFluxFr#.dat"]
c  Note: RadFlux(iv) is computed in SUBR "RTeSpIn()" but I moved the print here because 
c  SUBR "RTeSpIn()" works at each t, but TX scan is needed only at six tres(#) .

	write(60+FrNu,'(a38)') 'hvEV       iv   RadFluxW    KapD' ! "RadFlux(hv)" [W/eV/sr] is known in full hv grid,          
      do iv= 1, nvM
         write(60+FrNu, '(f12.2, i7, 9e11.4)') hvV(iv), iv,              ! eV;  
     +                             RadFlux(iv), abTot(2,iv,1)*2*R2       ! [W/eV/sr] from 1 BS; Opacity along central LOS of BS (La=2), 3rd index = 1 means motionless
      enddo
      close(60+FrNu)

c  hv-scans along thru-BS LOS done via "SpOut1(iv)" [W/sr/eV/cm2] calculated in SUBR "RTeSpIn" as 
c  "SpOut1(iv)"= RadFlux(iv)/(pi*R2^2); RadFlux is the 10-r-zone  sum
c  For comparison to TREX spectra, add contributions fron higher (here: 2,3,4,5) orders of mica reflection

      ReTX   = zero  ! TREX respo (product) in each order of hvV(iv)
      do iv = iTX1, iTX2  ! on v-grid of the code BUT only within v-interval where TREX response (product) is given    
        hveV= hvV(iv)	    ! find values of "ReTX(order, hvPoint) in hv-grid points
        do i2 = 1, npTX-1                                        ! hv-points in input file
           if(hvTX(i2).le. hveV .and. hveV.lt.hvTX(i2+1)) then   ! captured i2 & i2+1 TX point## around "hveV" 
              frac= (hveV -hvTX(i2))/(hvTX(i2+1)-hvTX(i2))       ! for linear interpolation
              do j = 1, 5                                        ! for each order of mica refl
                ReTX(j,iv)= RespTX(j,i2)+                        ! TREX product (in each order of refl) in hv-points of our hv-grid
     +                     (RespTX(j,i2+1)-RespTX(j,i2))*frac     
              enddo
           endif	
        enddo   
      enddo   ! iv

      SpOutOr= zero       ! "SpOut1" taken at j*hveV; j=1,2,3,4,5 , then plotted vs hv 
      do iv = iTX1, iTX2  ! on hv-grid of the code BUT only within v-interval where TREX response (product) is given    
        hveV= hvV(iv)
        do j = 1, 5             ! for each order of mica refl
          do i2 = iTX1-2, nvM-1    
            if(hvV(i2).le. j*hveV .and. j*hveV.lt.hvV(i2+1)) then  ! captured v-grid point## (i2 & i2+1) around j*hveV 
              frac= (j*hveV -hvV(i2))/(hvV(i2+1)-hvV(i2))          ! for linear interpolation
              SpOutOr(j,iv)=  SpOut1(i2  ) +                       ! SpOut1 at j*hveV; j=1,2,3,4,5; "SpOut1(iv)" is 10-r-zones area-mean SpIn from BS [W/sr/eV/cm2].
     +                       (SpOut1(i2+1) - SpOut1(i2)) *frac 
                MCPor(j,iv)= SpOutOr(j,iv) * ReTX(j,iv)/ReTX(1,iv) ! on-TX-MCP contrib of j's order. 
c                                                                  Note 1: here the units are [W/eV/sr/cm2] like "SpOut1" 
c                                                                  Note 2: "SpOut1(iv)" == RadFlux/(piR2^2) is 10-r-zones area-mean SpIn from BS [W/sr/eV/cm2].
c                                                                  Note 3: here "/ReTX(1,iv)" is Itsic's & Sandia algorithm, see "YM explanation to compu TREX scans".  
c                                                                          This "/ReTX(1,iv)" provides phys units [W/eV/sr/cm2] for "MCP", see 3 lines lower 
            endif	
          enddo   ! i2 over v-grid
          MCP(iv) = MCP(iv) + MCPor(j,iv)    ! on-TX-MCP at y=0: signal in [W/eV/sr/cm2] like 1st order + extra from 2,3,4,5 
        enddo  ! j
      enddo    ! iv of the code hv-grid within TREX interval = [1.28 - 1.93 keV]  

      Conv1= zero                ! [W/eV/sr/cm2] "MCP" convolved with Gaussian instrum function
      CALL ConvoTX(MCP, Conv1)   ! on-TX-MCP [W/eV/sr/cm2] due to 5 orders of mica, due to 1 BS  

      do iv= iTX1, iTX2		   
         if(hvV(iv) .lt. 1450.0) iv1450  = iv+1  ! conti left under Mg Ly-A peak, so far because of sats
         if(hvV(iv) .lt. 1472.3) iv1472p3= iv+1  ! Mg Ly-A-3/2 peak
         if(hvV(iv) .lt. 1485.0) iv1485  = iv+1  ! conti right under Mg Ly-A peak

         if(hvV(iv) .lt. 1695.0) iv1695  = iv+1  ! Point used for normalization of simulated conti to expt in B5
         if(hvV(iv) .lt. 1704.2) iv1704p2= iv+1  ! Peak of AL LyA 1D satellite "J"

         if(hvV(iv) .lt. 1740.)  iv1740 = iv+1   ! Red edge of MgLyB pede
         if(hvV(iv) .lt. 1744.6) iv1744p6= iv+1  ! Mg Ly-B peak
         if(hvV(iv) .lt. 1757.)  iv1757 = iv+1   ! Blue edge of MgLyB pede

         if(hvV(iv) .lt. 1758.)  iv1758 = iv+1   ! last point of small extra to ALLyA tail
         if(hvV(iv) .lt. 1765.)  iv1765 = iv+1   ! last point of ALLyA tail
         if(hvV(iv) .lt. 1770.)  iv1770 = iv+1   ! conti after MgLyB
      enddo

      PeakMgLyA = 0           ! after subtraction of underlying continuum.
      do iv = iv1450, iv1485  
        frac  = (hvV(iv)-hvV(iv1450)) / (hvV(iv1485)-hvV(iv1450)) 
        ContiA= Conv1(iv1450) + frac*(Conv1(iv1485)-Conv1(iv1450))  ! continuum under Mg LyA
        Conv1bb(iv) = Conv1(iv) - ContiA                            ! pure bound-bound shape of Mg LyA   
        if(Conv1bb(iv) .gt. PeakMgLyA) PeakMgLyA= Conv1bb(iv)	      ! [W/eV/sr/cm2]
      enddo
      HalfH = PeakMgLyA/2.  ! half-height of pure (bound-bound only) shape of Mg LyA 

      do iv = iv1450, iv1485          
        if( Conv1bb(iv)   .le. HalfH .and. 	     
     +	  Conv1bb(iv+1) .gt. HalfH ) hvRed =    !  Up along MgLyA shape 
     +      hvV(iv)+ (hvV(iv+1) - hvV(iv)) *      !  (...) > 0 
     +               (HalfH- Conv1bb(iv)) / (Conv1bb(iv+1)- Conv1bb(iv))  ! both parentheses > 0 
        if( Conv1bb(iv)   .gt. HalfH .and. 
     +	  Conv1bb(iv+1) .le. HalfH ) hvBlue=    ! Down along MgLyA shape 
     +      hvV(iv)+ (hvV(iv+1) - hvV(iv)) *      ! Note: (...) < 0
     +               (HalfH- Conv1bb(iv)) / (Conv1bb(iv+1)- Conv1bb(iv))  ! both parentheses < 0
      enddo
      FWHMA = hvBlue - hvRed   ! [eV] 

      ScaF= 2.42  /Conv1(iv1770)  ! 1520, Scaling Factor for Normalization of Conv1(iv) [W/eV/sr/cm2] to 
c                                         2.42 [arb.u.] in expt TX scan at hv=1770eV that is continuum on blue side of MgLyB 
c     ScaF= 1.3d-6/Conv1(iv1770)  ! 1907, Same for shot 1907 where expt = 1.3d-6 [arb.u.] at 1770eV in Fr A6,B1 

      write(170+FrNu,'(a29)') 'hvKeV     TXscan    KappaD' ! Full-D scan of TREX (based on RadFlux)
      do iv= 1, nvM
         if(hvV(iv).GT.1250. .and. hvV(iv).LT. 2100.)                    ! this interval is a bit wider tnan TX resolves
     +             write(170+FrNu,'(f9.6, 2e11.4)') hvV(iv)/1.d3,        ! keV;  											 ,
     +                                      Conv1(iv)*ScaF,              ! Signal [expt. arb.u.] on TX due to 5 orders of mica reflection,
c                                                                          convolved with TX instrumental function, normalized to 2.42 at 1770 eV 
     +                                      abTot(2,iv,1)* 2*R2          ! Opacity along central LOS of uniform spheric plasma (La=2), 
      enddo ! iv	                                                       ! 3rd index = 1 means "no Doppler shift"
      close(170+FrNu)
c      write(*,'(a25,i1, a12,i1, a3,i2)') 
c     +                  'Printed TX scan in (170+',  FrNu

      if(FrNu.LT.mSpe) write(*,'(/a50)')
     +       'tf[ns]  D[mm]   Te[eV]  TD[keV]    i/cc     %Mg'  

      if(FrNu .eq. mSpe) then  ! The end of the computations
             write(*,'(/a55)') 'Finished the Computations.'  
             STOP 	
      endif
      Return
      END     ! of subr "TXscans(FrNu)"


 

      SUBROUTINE PowYie()  ! estimate radiation Power [TW] from 1 BS..  
      use mo1
      implicit none   
      integer it, itL, Jpo 
      real(8) dw(JpoMx), dwhv(JpoMx), shapeJ(JpoMx), ShapeJV(nvM),      
     +        ShapeJVTD(nvM), ShapeJVTD10(nvM), ShapeJVTD10i(nvM),       
     +        tauC(nvM), AabLyB(nvM), AabLyB32(nvM), AabLyB12(nvM),   
     +        abLyB(nvM), Pow1k, PwBe, Fpow, Fpow0, Pow5, Pw10, Pw40,   
     +        RespBe, Resp40, Pow277, rw, FpowPr, Respo5,   ! Resp277
     +        Resp10, neJ, GauTD, pathBS, ReAr, expw, qBS,  
     +        ex40to10, P40toP10, alfaLyB12, alfaLyB32,
     +        ArJV, ArJVTD10, ArJVTD10i
	 
      PwBe  = zero   ! [W]: v-integral of "RadFlux" thru Be-filter in 4pi sr  
      Fpow  = zero   !                 full v-range   
      Fpow0 = zero   !                 full v-range in optically-thin limit   
      Pow1k = zero   !                 above 1 keV 
      Pow5  = zero   !                 thru 5 mils Kapton PCD filter
      Pw10  = zero   !                     10
      Pw40  = zero   !                     40
      Pow277= zero   !                 Respo of 277eV pinhole camera

      do iv= 2, nvM 
         do it=1, npBe8-1            ! use 8mcm Be filtered PCD response data to find the respo for current "iv"  
            if(hv8Be(it).lt.hvV(iv) .and. hvV(iv).le.hv8Be(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv8Be(itL))/(hv8Be(itL+1)-hv8Be(itL))		              		   
         RespBe= RespoBe(itL)+ Frac*(RespoBe(itL+1)-RespoBe(itL))     ! Response of Be-filtered PCD for "iv"

c  5 mils Kapton filter transmission
         do it=1, np5kap-1                                            ! use 5 mils Kapton filter transmission data array to find the transmission for current "iv"  
            if(hv5kap(it).lt.hvV(iv) .and. hvV(iv).le.hv5kap(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv5kap(itL))/(hv5kap(itL+1)-hv5kap(itL))		              		   
         Respo5= Resp5kap(itL)+ Frac*(Resp5kap(itL+1)-Resp5kap(itL))  ! Response of PCD filterd with 5 mils Kapton, for "iv"

c  10mils Kapton filter transmission
         do it=1, np10ka-1                                            ! use 10mils Kapton filter transmission data array to find the transmission for current "iv"  
            if(hv10ka(it).lt.hvV(iv) .and. hvV(iv).le.hv10ka(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv10ka(itL))/(hv10ka(itL+1)-hv10ka(itL))		              		   
         Resp10= Resp10ka(itL)+ Frac*(Resp10ka(itL+1)-Resp10ka(itL))  ! Response of PCD filterd with 10mils Kapton, for current "iv"

c  40mils Kapton filter transmission
         do it=1, np40ka-1                                            ! use 40mils Kapton filter transmission data array to find the transmission for current "iv"  
            if(hv40ka(it).lt.hvV(iv) .and. hvV(iv).le.hv40ka(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv40ka(itL))/(hv40ka(itL+1)-hv40ka(itL))		              		   
         Resp40= Resp40ka(itL)+ Frac*(Resp40ka(itL+1)-Resp40ka(itL))  ! Response of PCD filterd with 40mils Kapton, for current "iv"

         dhv= hvV(iv)-hvV(iv-1)

c    hv-integration for Powers from 1 BS. Remember:
c    RadFlux(iv) is 10-belts sum of BeltArea * ThruBelt Spin(iv) [W/eV/sr] from 1 BS;  . 
c    Present estimate of powers is based on single side-on detector like in the experiment.  
c    Detector covers solid angle "OmDet" thus collects PowDet[W] = OmDet[sr]* vInteg of Response*RadFlux[W/eV/sr]. 
c    Full (4pi) solid angle is larger than "OmDet" by factor "4pi/OmDet", thus 
c    full Power[W] = 4pi[sr]* vInteg of {Response * RadFlux[W/eV/sr]}

         Fpow  = Fpow + RadFlux(iv)  *dhv          *FoPi  ! [W]; Respo = 1, for Rad Loss and computation of Ti 
         Fpow0 = Fpow0+ emTot(2,iv,1)*dhv *Volu(2) *FoPi  ! [W]; Full v-range from-BS power in optically-thin limit 
         PwBe  = PwBe + RadFlux(iv)  *dhv *RespBe  *FoPi    
         Pow5  = Pow5 + RadFlux(iv)  *dhv *Respo5  *FoPi         
         Pw10  = Pw10 + RadFlux(iv)  *dhv *Resp10  *FoPi      
         Pw40  = Pw40 + RadFlux(iv)  *dhv *Resp40  *FoPi         
c        Pow277= Pow277+ RadFlux(iv)  *dhv *Resp277 *FoPi 
         if(hvV(iv) .gt. 1.d3)                            
     +   Pow1k = Pow1k + RadFlux(iv)  *dhv          *FoPi      
      enddo  ! iv  

      RadY   = RadY + tstep* (Fpow+FpowPr)/2.  ! Radiation Yield [J] in full spectral range, based on 1 side-on detector
      FpowPr = Fpow
 
      write(100,'(f7.4, 19e11.4)') tf*1.d9, Fpow/1.d12, PwBe/1.d12,      ! [TW] in full range, in 4pi thru Be-filter;  
     +                          Pow5/1.d12, Pw10/1.d12, Pw40/1.d12,      ! [TW] thru three hard PCD filters (5, 10, 40mils Kapton)  
     +                                         RadY, Te/1.d3, DenI       ! yield [J]

      write(280,'(f7.2, e11.4)') Te/1.d3, Pw40/Pw10  

********** Compu Ti via the electron energy eq. Do it here because fresh value of Rad POWER needed *******

c  Coulomb Log: CouL= Log(Pmax/Pmin); we use Spitzer's (5.14) 
c  Note: Braginski (& Golant after him) lost "-Ln(Zi)" in their final expression (or considered H). 
      CouL= 23.46+ 1.5*log(Te)-0.5*log(Dene)-log(ZC1)             ! in Fortran log==ln; log10==lg
      if(Te.GT .40.) CouL= 25.25+ log(Te)-0.5*log(Dene)-log(ZC1)  ! ZC1 is mean ion charge in the cell: average over XE & SS (including atoms), here from previous t-step
      if(CouL.LT.5.) CouL= 5.
c  typical time betw two elastic scatterings of an electron on ions [Braginskii.2.5.e], 
c  i.e. momentum-transfer time is
      teiBr= 3.441d5* Te**1.5 /CouL /Dene /ZC1  ! s, expr (10) in "EnEq" above;  Dene==ni*ZC1
      teiE = teiBr*MeanM/2./elma                ! s, is ei energy transfer time     

      if(tf .gt. strt+ 2.*tstep ) TeDot= (Te - TePrCO)/(tf-ti)  ! [eV/s] 
      TePrCO= Te	

c  3rd term in eq (Ti-Te)/teiE = dTe/dt + 2*Fpow/3/V1/ne/kB  ! [eV/s]; kB==BolJ in [J/eV] 
      TeDotPow  =  2.*Fpow /3./Volu(2)/Dene/BolJ   !  [eV/s] per 1 e

      TiCor   = Te + teiE* (TeDot + TeDotPow)  ! Ion temperature [eV]   
      if(TiCor .le. 0.) STOP 'TiCor <= 0'
   
c  Print Resume here, because it requires POWERs  ************************************* 

      if(ng .ne. gpEq(PrFr)) goto 12   ! "ng" in  the NUMBER of "tf"-point in t-grid; "gpEq(PrFr)" is # of t-point at which t = tres(PrFr)
c                                      ! "12" is the end of subr "PowYie"  
      write(40,'(/a9, f6.0, a3)')  'BS of D =', 2.d4*R2, 'mcm' 
      write(40,'( a4, f6.0, a9, e8.3, a12, f4.0, a3)') 'Te =', TeBS,   
     +             'eV,  ni =', DenI, 'i/cc,  TD =', TiD/1000.,'keV' 

      write(40,'(a6, f7.3, a6, f6.0, a14, e8.3)')   'got Z=', ZC1,  
     +                  ', Ti =', TiCor, 'eV,  mBS/3mg =', MaBSvsMw(2)
    
                               qBS= expPCD40/Pw40      ! *************** qBS *************** 

      write(40,'(a6, f4.2, a17, e9.3, a10)') 
     +    'PCD40=', expPCD40/1.d12, 'TW is emitted by', qBS, 'such BSs.'

      write(40,'(a9, f5.0, a16)') 'They emit',
     +                   100.* qBS * Pw10/expPCD10, '% of expt PCD10'  

      write(40,'(f14.0, a16)') 100.* qBS * PwBe/expPCDBe, 
     +                         '% of expt PCDbe'    

      write(40,'(f14.0, a14)') 100.* qBS * Fpow/expXRD,  '% of expt XRD'  

      write(40,'(a31, f7.2, a8)') 'Total mass of these BSs, Mx, is', 
     +                               100*qBS*MaBSvsMw(2), '% of Mw.'

      Vx = qBS*Volu(2)*1000. ! [mm^3]
      write(40,'(a23, f6.2, a5)')    
     +               'Their total Volume Vx =', Vx, 'mm^3'
      write(40,'(a37, f5.1, a3)')    
     +               '= Volume of cylinder of D = 1 mm, L =', 
     +                           10.*qBS*Volu(2) / (pin*0.05**2), 'mm'   ! 10 converts V[cc], R=0.05cm to L[mm]

      P40toP10 = Pw40/Pw10
      ex40to10 = expPCD40/expPCD10
      if(0.79*ex40to10 .LT. P40toP10 .and. 
     +         P40toP10 .LT. 1.21*ex40to10) then 
         write(40,'(a11, f6.4, a22, f6.4)') 'My P40/P10=', Pw40/Pw10, 
     +                                 'is WITHIN 21% of expt', ex40to10   
      else
         write(40,'(a13, f6.1, a28)') 'My P40/P10 is',  
     +     100.*(Pw40/Pw10)/ex40to10, '% of expt, thus TOO MUCH OUT'    
      endif           

      if(MgLyAmin .LT. FWHMA .and. FWHMA .LT. MgLyAmax) then 
        write(40,'(/a11,f5.2, a19, f4.1,a2,f4.1)') 'MgLyA FWHM=', FWHMA, 
     +                    'eV  FITS Min-Max =', MgLyAmin, '-', MgLyAmax  
      else
        write(40,'(/a11,f5.2, a21, f4.1,a2,f4.1)') 'MgLyA FWHM=', FWHMA, 
     +                 'eV is OUT of Min-Max=', MgLyAmin, '-', MgLyAmax     
      endif 

      write(40,'(a44)') 
     +         'MgLyA FWHMs: Gauss  Natur  Stark  Lor  Voigt' 
      write(40,'(a15,f7.3,f6.3,f7.3, f6.3, f7.3)') '[eV]', MgLyA_FW_Gau, 
     +       MgLyA_FW_Natu, MgLyA_FW_StJe, MgLyA_FW_Lor, MgLyA_FW_Voi

****************************************************************************************************
Compute MgLyB shape after each transformation mentioned on page 6 of the Report, also see Fig 16. 
c  Print these shapes in file (371. Derive FWHM of MgLyB. Print it in Resume.dat, file (40.

c  Initial comments:     
c  1. Each (of 4) from-Jenya Stark shapes has 2 peaks. The shapes given in function of (w-w0) where
c     "w" = 10^8 [A/cm] / Lambda [A] is waveNumber. Jenya: w0 = 14072834 1/cm, Lambda0 = 7.10589 A". .
c     Lambda0 is centre of MgLyB. Really, eVA/Lambda0 = 12398.424 [A*eV] / 7.10589 [A] = 1744.809 eV close
c     to NIST g-mean energy = 1744.746 eV.
c  2. Single ion has dE = 0.28eV between two J-components of MgLyB while Jenya's shape has dw = 2d4 cm^-1 	  
c     that gives dE = h*c*dw = 4.136d-15 [eV*s] * 3d10 [cm/s] * 2d4 [1/cm] = 2.5 eV because  
c     in.dense plasma both peaks are mixture of 3s, 3p, 3d levels.
c  3  In the title of Jenya's file "deg" means degenerate plasma.   

      if(Dene .LE. 2.d22) then																					
         open(901, file= 'Mg_XII_1.3e22_1300_2500_deg.dat')  !  This file contains E.S.' shape (J-shape) of MgLyB
c                         as a function of (w-w0) [1/cm]; "w" is the WaveNumber = 10^8 [A/cm] / Lambda [A] 
c                         The shape is norm to "top=1". Note: FWHM of these accurate J-shapes fit WIS-formulary
         Jpo = 565        ! number of points Jenya's w-grid
         neJ = 1.3e22     ! ne in the file 
      endif
	  
      if(Dene .GT. 2.d22 .and. Dene .LE. 4.d22) then
         open(901, file= 'Mg_XII_2.6e22_1300_2500_deg.dat')  
         Jpo = 671 
         neJ = 2.6e22     
      endif

      if(Dene .GT. 4.d22 .and. Dene .LE. 8.d22) then
         open(901, file= 'Mg_XII_5.2e22_1300_2500_deg.dat')  !
         Jpo = 445 
         neJ = 5.2e22     
      endif

      if(Dene .GT. 8.d22) then
         open(901, file= 'Mg_XII_1.04e23_1300_2500_deg.dat') 
         Jpo = 705  
         neJ = 1.04e23     
      endif
  
      do j = 1, 8
        read(901, '(a10)') title  ! 8 lines of Jenya's comments      
      enddo				

**************************** Conversion to [eV] hv-grid ********************************
      do j = 1, Jpo                      !  
         read(901,*) dw(j), shapeJ(j)       ! "dw(j)" is j-th point of Jenya's (w-w0)-grid [1/cm] which is centered to w0 = 0 
         dwhv(j) = 1744.75 + c* h* dw(j)    ! [eV] My "dwhv(j)" is Jenya's "dw" converted to hv[eV] and posed around FAC DaBa center of MgLyB
c											c[cm/s]* h[eV/Hz] (w-w0)[1/cm] has units [eV]
     +             * ((Dene/neJ  )**0.578)  ! scaling factor computed via ne-scaling of FWHM of 3 J-shapes (namely, 
c                                             40310, 59871, 90698 cm^-1 for Z=13, ni = (1, 2, 4)x10^21. Done Nov 5, 2022
     +             * ((TeBS/1000.)**0.324)  ! According to J-formulary of Nov 4, 2022, assuming 4% Mg in Al plasma of Z=13, Ti= 2Te,   
c                                             The interval is [Te= (1-1.4) keV, ni = (1-3}d21 i/cc].
c                                             Run with this scaling gave Stark FWHM= 6.26 eV at Te= 1.0 keV, ni= 1.5x10^21 i/cc, Ti= 2.03 keV, Z=12.74
c                                                                                    9.27 .. at Te= 1.0 keV, ni= 3.0x10^21 i/cc, Ti= 1.93		 Z=12.76
c                                                                                    7.01 .. at Te= 1.4 keV, ni= 1.5x10^21 i/cc, Ti= 2.90		 Z=12.84
c                                                                                   10.39.   at Te= 1.4 keV, ni= 3.0x10^21 i/cc, Ti= 2.76		 Z=12.85
c                                             vs 6.40, 9.17, 7.05, 10.65 eV in WIS Formulary, which is less accurate.and uses Z= 13.
      enddo	                                
      close(901)

Convert "shapeJ(j)' to "ShapeJV(i)", which is approximately the same shape but on my full hv-grid 
      ShapeJV = 0.d0                 ! Note: this is pure MgLyB Stark, BB only, NORMALIZED TO PEAK = 1  
      do i = 1, nvM   ! my hv-grid
      do j = 2, Jpo   ! Jenya's points, except the 1st point because (j-1) will be needed; 
         if(hvV(i) .GT. dwhv(j-1) .AND. hvV(i) .LT. dwhv(j)) then    ! my point "i" is between two j-points
            Frac= ( hvV(i)-dwhv(j-1) ) / ( dwhv(j)-dwhv(j-1) )
            ShapeJV(i)= shapeJ(j-1) + Frac * ( shapeJ(j)-shapeJ(j-1) ) 
         endif
      enddo
      enddo

c   Normalize to area = 1, because will be used for absorp coeff
      ArJV = 0.                 ! compute Area under each shape
      do k3 = iTX1, iTX2-1 
         dhv  = hvV(k3+1)-hvV(k3)
         if(ShapeJV(k3) .gt. 1.d-7)    ! "ShapeJV" is norm to peak = 1 but does not cover full [iTX1 - iTX2]
     +   ArJV = ArJV + (ShapeJV(k3)+ShapeJV(k3+1))*dhv/2. 
      enddo
      do k3 = iTX1, iTX2-1 
         ShapeJV(k3) = ShapeJV(k3)/ArJV
      enddo

      CALL FindFWHM( ShapeJV, FWHM_JV)  ! Find FWHM of function "ShapeJV"  [only for print (40 , that is "Resume"].      

Convolve "ShapeJV(i)" with Gaussian of FWHM caused by TD, i.e. by (a) Ti-motion of ions in BS, 
c                     (b) 3D motion of BSs, (c) hydro in BSs (if any). 
      ShapeJVTD= 0. ! will be J-Stark G-scaled "ShapeJ" posed on my full hvV(i) grid, then convolved with TD-Gaussian.
c                     Ti-Doppler has FWHM= hvC* 7.7155d-5 *sqrt(Ti/A)  [my page = NIST formu];
c                     Simiarly, for TD using Mg A=24.3 one obtains MgLyB TD-Gaussian FWHM =  
c                     = 0.0273*sqrt(TiD) [eV] 	 
      GauTD = 0.0273*sqrt(TiD)/1744.7  ! FWHM/hvC

      CALL GauConvo(ShapeJV, ShapeJVTD, GauTD, 3)  ! means Convolve "ShapeJV" with Gaussian in "case 3" in which
c                                                  3rd param is FWHM/hvC[eV] like ~0 001 in Ti-Doppler.
c             Convo with Gau keeps NORM TO AREA=1 done for "ShapeJV(k3)" 

      CALL FindFWHM( ShapeJVTD, FWHM_JVTD)   ! Find FWHM of function "ShapeJVTD"  [only for print (40 , that is "Resume"],   

c  Spectral line from optically thick BS is broadened by opacity along a LOS.  
c  BS is  spherical,thus, TX sees round cross-section of area= pin*R2^2.   
c  I split this area in 10 r-belts & take 10 LOSs, each thru the middle of a belt towards TX, 
c  Opacities differ. Resulting (full area) shape is called "ShaJVTD10"  
     
      do i= iTX1, iTX2-1 
        if(hvV(i) .LT. 1700) cycle
        if(hvV(i) .GT. 1790) cycle
        AlfaLyB12 = 0.     ! relates to Mg LyB 1/2 
        AlfaLyB32 = 0.     ! relates to Mg LyB 3/2  
        if(POPt(93,2,2).gt.1.d-15) then          ! "93,2,2" is (kL, nX, La) of the Gr.St.of H-like Mg in BS
	    AlfaLyB12= (POPt( 98,2,2)/g0( 98,2)) / (POPt(93,2,2)/g0(93,2))  ! QS #98  is upper of Mg LyB 1/2
	    AlfaLyB32= (POPt(100,2,2)/g0(100,2)) / (POPt(93,2,2)/g0(93,2))  ! QS #100 is upper of Mg LyB 3/2
        endif
        AabLyB12(i)= 1.098d-16* 0.0264 *POPt(93,2,2)*Den(2)  ! [eV/cm] Amplidude of absorptivity caused by Mg LyB1/2 only. BB only. 
        AabLyB32(i)= 1.098d-16* 0.0528 *POPt(93,2,2)*Den(2)  ! i.e. Absorptivity (16) except *P(hv); i.e. [eV/cm], see (18)   
	  AabLyB(i)  = (AabLyB12(i) + AabLyB32(i) )/2.           ! mean of 2 components because Jenya's "ShapeJVTD(i)" includes both components of Mg LyB 
	   abLyB(i)  = AabLyB(i) * ShapeJVTD(i)                ! [1/cm] BB absorption coeff for Mg LyB only. 2 components of Mg LyB          
c                               "ShapeJVTD" is norm to area = 1, like my renorm of Jenya's shape                               
        ShapeJVTD10(i)= 0.    
        do k = 1, 10            ! round belts
           rw= (R2/10.)*(k-0.5)             ! y of side-on LOS
           pathBS = 2.*sqrt(R2**2 - rw**2)  ! thru-BS path of k-th LOS; 
           expw = exp( -abLyB(i)*pathBS )   ! exp factor for attenuation in BS (La=2) due to BB abso by Mg Ly-B ONLY, 
c                                             no wing of ALLyA, like subtracted in Figs 13, 14 of the Report, .  
           ReAr = (k**2- (k-1)**2)/100.      ! Relative Area of k-th ring 		   
           ShapeJVTD10(i)= ShapeJVTD10(i)+ ReAr*ShapeJVTD(i)*(one-expw)  ! Mg Ly-B shape towards TX
        enddo 
        tauC(i) = 2.*R2* abLyB(i)  ! opacity along central LOS; This opacity is due to Mg LyB only 
      enddo 

c  Normalize "ShapeJVTD10" to area = 1
      ArJVTD10 = 0.
      do k3 = iTX1, iTX2 
         dhv= hvV(k3+1)-hvV(k3)
         if(ShapeJVTD10(k3) .GT. 1.d-7) ArJVTD10 = 
     +	      ArJVTD10 + (ShapeJVTD10(k3) + ShapeJVTD10(k3+1))*dhv/2. 
      enddo
      do k3 = iTX1, iTX2-1 
  	   ShapeJVTD10(k3) = ShapeJVTD10(k3) / ArJVTD10 
      enddo
	   
      CALL FindFWHM(ShapeJVTD10, FWHM_JVTD10)   !  Find FWHM of function "ShapeJVTD10 [only for print (40 , that is "Resume"].    

Convolve "ShapeJVTD10)" with instrumental Gaussian 

      ShapeJVTD10i = 0. 
      CALL ConvoTX(ShapeJVTD10, ShapeJVTD10i )  

c  Normalize "ShapeJVTD10i" to area =1 for comparison in plots
      ArJVTD10i = 0.
      do k3 = iTX1, iTX2 
         dhv= hvV(k3+1)-hvV(k3)
         if(ShapeJVTD10i(k3) .GT. 1.d-7) ArJVTD10i = 
     +	    ArJVTD10i + (ShapeJVTD10i(k3)+ ShapeJVTD10i(k3+1))*dhv/2. 
      enddo
      do k3 = iTX1, iTX2-1 
  	   ShapeJVTD10i(k3) = ShapeJVTD10i(k3) / ArJVTD10i 
      enddo

      CALL FindFWHM(ShapeJVTD10i, FWHM_JVTD10i)  !  Find FWHM of function "ShapeJVTD10i",.   
c                                                Note: FWHMs of MgLyB shape are needed only for "Resume.dat"; print (40. 

c  Print normalized (to area = 1) shapes of Mg LyB:
	   write(371,'(a63)')  
     +    'hvEV     ShapeJV   ShapeJVTD  ShaJVTD10  ShaJVTD10i   KappaD' 
         do k3 = iTX1, iTX2-1 
            if(ShapeJV(k3) .GT. 1.d-4) 
     +      write(371,'(f9.3, 4e11.4, f9.4)') hvV(k3), ShapeJV(k3),  
     +                ShapeJVTD(k3),	ShapeJVTD10(k3), ShapeJVTD10i(k3),
     +                tauC(k3)
         enddo     
         close(371)

      write(40,'(/a46)')'MgLyB FWHM after 3 transformations of JV shape'
      write(40,'( a48)') 'JV    JVTD    JVTD10  JVTD10i =' 
      write(40,'( f21.3, 3f8.3)') FWHM_JV,  FWHM_JVTD,  FWHM_JVTD10,
     +                                                  FWHM_JVTD10i
      FWHMB = FWHM_JVTD10i 

      if(MgLyBmin .LT. FWHMB .and. FWHMB .LT. MgLyBmax) then 
         write(40,'(a11,f5.2, a18,f4.1,a2,f5.1//)') 'MgLyB FWHM=',FWHMB, 
     +             'eV  FITS Min-Max=', MgLyBmin, '-', MgLyBmax  
      else
         if(FWHMB .LE. MgLyBmin) 
     +   write(40,'(a11, f5.2, a18, f4.1//)') 'MgLyB FWHM=', FWHMB, 
     +                                'eV is BELOW Min =', MgLyBmin  
         if(FWHMB .GT. MgLyBmin) 
     +   write(40,'(a11, f5.2, a18, f4.1//)') 'MgLyB FWHM=', FWHMB, 
     +                                'eV is ABOVE Max =', MgLyBmax     
      endif  
  12  Return					  
      END    ! of 'PowYie' subr

       

      SUBROUTINE AtKins() ! Atomic Kinetics for current La, nX.  On entry POP(QS#) given POPt(k,nX,La) from previous t-step (t=ti)
      use mo1             ! on exit POP(QS#) contains the distribution at t=tf and re-writes POPt(k,nX,La) for current La, nX
      implicit none
      real(8)  POP(NSTm), POPti(NSTm)  ! these files are Local in AtKins
      external POPdot     ! subroutine mentioned by name (no argument) in d02

      CALL redPI()        ! find Ioniz Potentials for {Te(ti-tf),ni(ti-tf)} of MAIN: PIR(SS,nX)= PI(SS,nX)-dPI(SS,nX)
      CALL BEvPr()        ! find Binding Energy & Price of QSs

      Sah = Dene*(one-bp)/(6.0371288d21*Te**1.5d0)  ! for 3-body recombination

      do k= 1, NST(nX)
        POP(k) = POPt(k,nX,La)   ! shorter name. POPs from previous t-step
      enddo

CUT-off block
      do k= 1, NST(nX)
        if(POP(k).lt.-1.d-12) STOP 'came to cut-off with POP < -1.d-12'  ! NAG d02 with tolD02= 1.d-6 provides |sumPOP - 1| < 1.e-8 but some small POPs may be negative |POP|<1.e-12
        if( k.LT.Nnu(nX) .AND. BE(k).lt. 1.d-3                           ! dead non-AI QS
     +                   .AND. POP(k).gt. zero) then  ! cut it;  Note: (i) AIQSs have BE = -13., I keep them "never dead"  
          POP(nuGS(kiSS(k,nX)+1 ,nX)) = 
     +    POP(nuGS(kiSS(k,nX)+1 ,nX)) + POP(k)        !  Add it to GS of next SS
          POP(k)= zero                                !  empty cut QS 
	  endif
        POPt(k,nX,La) = POP(k)  ! update because "POP" is local here while subr "Ws" uses "POPt(k,nX,La)"
      enddo

Comment on RESTART of dead 'k' level:  when BEp(k,XE,Layer) <0 changes to BE(k)>0, then initial POP(k)==0 is  given proper value in D02

      CALL Ws()  ! calculate all transition probabilities mentioned in Rate Equations
      CALL PMg0  ! PM(k,kf) and Wout(k,nX,La) for g0 case (table ddegeneracy)	

      do k= 1, NST(nX)
          POPti(k)= POP(k) 
      enddo

c  Integrate the rate equations:
      ifail= -1
      Scale= 1.d40
      tiS= ti*Scale
      tfS= tf*Scale

      CALL D02EAF(tiS, tfS, NST(nX), POP, tolD02, POPdot, WEAF, Nwork, 
     +            ifail) 

***   After-D02 printout in "Comment.dat". Only in PrFr time:               
      if(ng .eq. gpEq(PrFr)) then  ! "ng" is the NUMBER of "tf"-point in t-grid
        write(41,'(//a7, f5.3, a20)') 'tf=', tf*1.d9, 
     +                                   'ns.  POPs after d02'
        do k= 1, NST(nX)   
          if(k.eq.1) write(41,'(/a56)')       'XE  QS#   QSname        E     
     +        g0      Wout      POPtf' 
          write(41,'(i2, i5, a6,a5,   f11.3, f7.0,    2e11.4)') 
     2		       nX, k, QSname1(k,nX), QSname2(k,nX), 
     3               E(k,nX), g0(k,nX),     -PM(k,k), POP(k)
        enddo 
      endif   

check POPs obtained:
      SumP= zero 
      do k= 1, NST(nX)
	  if( k.LT.Nnu(nX).AND. BE(k).LT. 1.d-3  
     +                  .AND.POP(k).GT. 1.d-15) STOP 'Populated dead QS'
	  if(POP(k) .LT.-1.d-12) Write(*,'(a29,e10.2, a17,i2, i4,e14.6)')  ! NAG d02 with tolD02= 1.d-6 provides |sumPOP - 1| < 1.e-8  but some small POPs may be negative |POP|<1.e-12
     +     'found POP < -1.d-12;  POP=', POP(k),                         ! If these negative values are < -1.e12 we send screen info 
     +     'for XE, QS, BE=', nX, k, BE(k)			  
        sumP= sumP+ POP(k)
      enddo
      if(abs(SumP-one) .gt. 1.d-6) then
         write(*,'(/a20, e9.2)') 'SumPOPs - 1 =', SumP - one 
         STOP 'Sum POPs ne 1 after d02'
      endif

calculate POPZ(SS) 
      do jSS= FSS(nX), HSS(nX)+1
        POPZ(jSS,nX,La)= zero      ! relative abundance of ionization stage
      enddo
      do k= 1, NST(nX)
        POPZ(KiSS(k,nX),nX,La)= POPZ(KiSS(k,nX),nX,La) + POP(k) 
      enddo

      if(nX.eq.1) write(21,'(f7.4, f8.3, 6e11.4)') tf*1.e9, Te/1000.,
     +       POP(PoLeNu(1,1)), POP(PoLeNu(1,2)), POP(PoLeNu(1,3)),	   ! POP of six levels of AL chosen "FLAG.inp".
     +       POP(PoLeNu(1,4)), POP(PoLeNu(1,5)), POP(PoLeNu(1,6))
     
      if(nX.eq.2) write(22,'(f7.4, f8.3, 6e11.4)') tf*1.e9, Te/1000.,
     +       POP(PoLeNu(2,1)), POP(PoLeNu(2,2)), POP(PoLeNu(2,3)),       ! POP of six levels of AL chosen "FLAG.inp".
     +       POP(PoLeNu(2,4)), POP(PoLeNu(2,5)), POP(PoLeNu(2,6))

c Re-load (save) 1D arrays POP(k) and BE(k) in 3D arrays for next t-step with this "La" & "XE" 
      EinzEx(nX,La) = zero  ! Per ion ioniz+Excit energy [eV]
      do k= 1, NST(nX)
         POPt(k,nX,La)= POP(k)
	    BEp(k,nX,La)= BE(k)
         EinzEx(nX,La)= EinzEx(nX,La)+ Price(k,nX) *POP(k)  ! [eV/ion]
      enddo

      Return
      END   ! of 'AtKins' subr



      SUBROUTINE redPI()  ! gives Ionization Potentials for {Te(ti-tf),ni(ti-tf)} of MAIN.  
	use mo1             ! two options: "table values (no lowering)", "ion-sphere", depending on "KeReDu" in Flag.inp
	implicit none
	real(8) SpheR, DPIsph(HSSm+1)

      if(ng .eq. gpEq(PrFr)) then   ! "ng" in  the NUMBER of "tf"-point in t-grid
         write(41,'(/a7, f5.3, a21)') 'tf=', tf*1.d9, 
     +                                         'ns is the PrFr time:'
         write(41,'(/a33)') ' SpS   tablePI    IonSphDPI    PIR'    
      endif     
     
      SpheR=(three/FoPi/DenI)**third   ! Ion Sphere radius from 1= 4piR^3*Ni/3; Gr3 (7.36)    

      do jSS = FSS(nX), HSS(nX)        ! all ionizible specroscopic symbols   
	  DPIsph(jSS) = 13.606*dble(jSS)* 1.5*0.5292d-8/SpheR  ! [eV], Griem3 (7.43)
  	  if(KeRedu .eq.-1) DPI(jss,nX)= zero          ! no lowering
  	  if(KeRedu .eq. 0) DPI(jss,nX)= DPIsph(jss)   ! ion sphere

        PIR(jSS,nX)= PI(jSS,nX) -DPI(jSS,nX)
        if(PIR(jSS,nX) .lt. PI(jSS,nX)/5.) stop '  PIR < PI/5.'

        if(ng .eq. gpEq(PrFr)) write(41,'(i3,3f11.3)') jSS, 
     +                            PI(jSS,nX),  DPIsph(jSS),  PIR(jSS,nX)
	enddo  
      Return
      END   ! of 'redPI' subr



      SUBROUTINE BEvPr()  ! gives Binding Energy of outermost electron (BE) & Price of QSs
	use mo1
	implicit none
	real(8) PriI(HSSm)         ! Ionization Price of GSs
      PriI(FSS(nX))= E(1,nX)     ! =E of 1st GS; it may be >0 if the GS is term
      do j= FSS(nX)+1, HSS(nX)+1  
        PriI(j)= PriI(j-1) +PIR(j-1,nX)  ! ION GS energy relative to E(1)
	enddo
      do k= 1, NST(nX)  
        BE(k)      = -13.d0  ! '-13' will show fogotten regular QS or AIQSs 
        Price(k,nX)= zero    ! QS energy relative to E(1,nX), stays zero for BE<0 
      enddo

      do j= FSS(nX), HSS(nX)  
         do k= nuGS(j,nX), nuGS(j+1,nX)-1   ! regular QSs of j
            BE(k)= PIR(j,nX) - E(k,nX)      ! above-PIR QSs obtains BE < 0
            if(BE(k).ge. 1.d-3) Price(k,nX)= PriI(j) +E(k,nX) 
	   enddo
      enddo
      BE(Nnu(nX))= 2.d-3                 ! [eV] Nucl given Artificial (wrong) small BE > 0 to  avoid its de-POP by "if(BE < 1.d-3)"
      Price(Nnu(nX),nX)= PriI(HSS(nX)+1) ! nucl

c     AIQSs have no BE. They can decay spontaneously
       
      do j= FSS(nX), HSS(nX)                ! don't include in previous j-loop
         if(nuAS(j,nX) .LT. 1) goto 11
         do k= kAI1(j,nX), kAI2(j,nX) 
             Price(k,nX) = PriI(j) +E(k,nX)  !    
	   enddo
 11	   continue
      enddo

      do k= 1, Nnu(nX)  !   it was until NST(nX) but now up to nucl because AIQSs have no BE.
        if(abs(BE(k)-13.d0).lt.3.d-8)  then 
	    write(*,'(2(a9,i4), a33)') 
     +         'QS#', k, 'of XE#', nX, 'not given binding energy BE(k)'
		STOP
        endif
      enddo
      Return
      END     ! of 'BEvPr' subr



      SUBROUTINE Ws()  ! Transition probabilities for Rate Equations.
      use mo1          ! In each W-matrix, first index shows INITIAL QS of the transition.
      implicit none
      integer  lUL, lw, kAI 
      real(8)  pv(nvM), freq, Wave, DEJ, BemHz, Bem, Bab, hvCw,  
     +         AbSpIn, up, rate, prob, Ejm, wFac, D01ahf,  
     +         SigRR, V, fun, SigPhi, eeV, dev, Voigt2,     ! Voigt2 is Sawa's connector to JQSRT Viogt; Earlier used "VoigtJS" that is Jenya's Voigt function but is was WRONG 
     +         FWevGau, EED, Sca, ArPv                      ! Gauss, 
      external FVSinz, FVSmi, FVStbr, FVSrr, FVSex, FVSdx   ! functions mentioned by name (no argument) in d01
      WI  = zero
      WMI = zero
      WEX = zero
      WDX = zero
      WRR = zero
      WTB = zero
      WInd= zero
      Wab = zero
      WPhI= zero
      WDC = zero
      WiRR= zero

*  Probability of bound-bound INDUCED emission & absorption: Wind(U,L) & Wab(L,U)
      do kL= 1, NST(nX)-1
      do kU= kL+1, NST(nX)
        if(A(kU,kL,nX) .LT. 1.)      goto 2  ! this kU,kL couple does non play: Weak lines & near-soft-edge lines got A==0 in Intro 
        if(kU.LT.Nnu(nX) .AND.               ! non-AI QS,  
     +     BE(kU).LT.1d-3)           goto 2  ! dead; Note: AIQSs are treated as "never-dead"; they have no true BE, thus carry initial BE = -13 
        if(POPt(kU,nX,La).LT.1.d-12) goto 2  ! dead of poor kU, out of interest  
        DE= E(kU,nX) - E(kL,nX)              ! photon energy, eV
        if(DE .LT. hvSmo)            goto 2  ! small-hv lines are (i) broad by Life-time, (ii) not seen above FF continuum
        if(DE .GT. hvMax*0.9)        goto 2

        freq= DE/h       ! photon frequency, Hz
        Wave= c/freq     ! cm
        DEJ = BolJ* DE   ! photon energy, J
c  'Bem' is Einstein B coef for induced emission in kU --> kL
        BemHz= A(kU,kL,nX)*Wave**2/two/DEJ   ! Hz*cm2*sr/J,  A*Lam^2/2hv(J), eq (6) tCRC PAP
        Bem= BemHz*h                         ! eV*cm2*sr/J;  units for * SpIn [W/sr/eV/cm2] --> [W/J]=1/s
c  'Bab' is Einstein B coef for absorption in kL --> kU line
        Bab= Bem*g0(kU,nX)/g0(kL,nX)         ! eq (7) tCRC PAP
c  'Wind(U,L)' & 'Wab(L,U)' are  B * integ{dv*p(v)*SpInEff(v)};  'p(v)' is kL-->kU absorption line shape
        lUL= -3                     ! # of kU-->kL line in the LineList
        do lw= 1, linM4             ! catch this "lUL"
           hvCw= hvC(lw)
           dev= abs(DE-hvCw)/DE
           if(dev .lt. 1.d-7) then
              lUL= lw
              goto 1
           endif 
         enddo
         write(*,'(a22, 2i5, 2f10.4)') 'kL, kU, DE, hvC=', 
     +                                  kL, kU, DE, hvC(lw)
         STOP 'Did not recognize the Line'
 1       continue
         if (lUL .lt. 1) then        ! not found
            write(41,'(a40, 3i5, f11.4, e11.3)') 
     +      'lUL < 1 for XE, kU, kL, DE, A =', nX, kU, kL, DE, 
     +                                             A(kU,kL,nX)
            STOP 'Line# not found'  
         endif

         if(FWevLor(lUL) .LT. 1.d-16) goto 2  ! "FWevNatu" =0 for dead levels becouse their "Wout(kU,nX,La)"==0
c                                               "StJen" exist only for hv > 1200 eV 
c                                     Thus many lines are formally present but their U is dead & FWevLor(lw)=0 
         FWevGau= hvCw *FWkVcGAU(nX)  ! Gaussian FWHM [eV] of kU-->kL line of XE# = nX
         Wing   = hvCw/2.             ! Line Wing Length [eV]
         ArPv   = zero
         do iv= 1, nvM 
            dev= abs(hvV(iv) - hvCw)
            pv(iv)= zero
            if(hvV(iv).GT. hvCw-Wing .and. 
     +         hvV(iv).LT. hvCw+Wing      ) then 
               pv(iv)= Voigt2(FWevLor(lUL), FWevGau, hvCw, dev)    ! JQSRT (Sawa) Voigt is OK to 1% for any L,G, but slower than wrong "VoigtJS"   
            endif                                                 
            if(iv.GT.1) ArPv = ArPv +                              ! Area under shape < 1 because line wings cut thus part of photons lost. 
     +       	          0.5*(pv(iv)+pv(iv-1))*(hvV(iv)-hvV(iv-1))  ! To return them, no-wings shape will be re-norm to Integ[pBB(v)dv] = 1
         enddo                                                     ! see "/ArPv" below
	                                                   
         if(ArPv .LT.1.d-5) then				   
            write(*,'(a26, i6, f10.3, 9e10.2)') 
     +      'lw, hvC, FWevGau, ArPv =', 
     +       lw, hvC(lw), FWevGau, ArPv
	      STOP '  ArPv < d-5)' 
         endif

         AbSpIn = zero  ! v-integral of {dv*p(v)*SpInEff(v)} [W/eV/cm2/sr]
         do iv= 2, nvM 
            dhv= hvV(iv)-hvV(iv-1)
            AbSpIn= AbSpIn + dhv*	SpInEff(La,iv)*pv(iv)/ArPv ! "ArPv" renorms no-wings under-shape areq to 1. 
         enddo
         WInd(kU,kL)= Bem*AbSpIn
         Wab (kL,kU)= Bab*AbSpIn
 2       continue
      enddo
      enddo

*  Ionization (removal of ONE electron) probability matrixes WI(k,kf), WPhI(k,kf) & backward WTB(kf,k), WRR(kf,k), WiRR(kf,k) .  
*                                                  
      up= 100.*Te                              ! Notations: (j,k) + e = (j+1,kf) +2e
      if(bp.gt.1.d-7) up= 100.*Te +bc +6.*bw

      do   k  = 1, LastAI(nX)                  
        do kf = 1, LastAI(nX)    
           if(bra(k,kf,nX) .LT. 0.5) cycle  ! no link;  "bra" is 0 or 1 with FAC bases 
           if(k .LT. Nnu(nX) .AND.          ! non-AI QS,  
     +        BE(k).LT.1.d-3)        cycle  ! this QS is dead; Note 1: Nucl given BE= 0.002 eV; Note 2: Nucl & AIQSs cannot be dead 
           if(kf.LT. Nnu(nX) .AND.          ! non-AI QS,  
     +        BE(kf).lt.1.d-3)       cycle  ! this QS is dead; leave WI(k,kf)= 0
           j = KiSS(k,nX)   
           BEk= Eth(k,kf,nX)-DPI(j,nX)      ! energy for "(j,k) + e = (j+1,kf) +2e" ionization
           if(BEk .GE. 0.99*up)      cycle  ! "Te" too low, see limits of d01 integral below 

           if(k.GT.Nnu(nX)   .AND.          ! AIQS. Because of big DPI this k=AI --> kf ionization
     +        BEk .LT. zero) BEk= 1.        ! does not require E, but I give formal 1 eV to avoid 0.  
c                                           Note: "BEk < 0" never happens for regular k because big DPI cuts it. 
           Ifail= -1
           rate= D01AHF(BEk, up, tol, NPTS, rer, FVSinz, Nlim, Ifail)  ! cc/s
           WI(k,kf)= rate * Dene            ! Probability, 1/s

           rate= D01ahf(1.d-7, up, tol, NPTS, rer, FVStbr, Nlim, Ifail) 
           WTB(kf,k)= rate * Dene   
		 
           prob = 0.                     ! probability of PHOTO-ionization 
           do iv= 2, nvM                 ! Explicit Sum instead of d01, which is too slow with multy-spike "SpInEff" 
             if(hvV(iv).GE. BEk) then                        
               dhv = hvV(iv) - hvV(iv-1)
               fun= FoPi*SigPhi(hvV(iv))*SpInEff(La,iv)/hvV(iv)/BolJ  ! 1/s/eV= sr*cm2*(W/cm2/sr/eV)/J ; BlackFold probability page 
               prob= prob + dhv * fun  
             endif
           enddo
           WPhI(k,kf)= prob   ! 1/s

           rate= D01ahf(1.d-7, up, tol, NPTS, rer, FVSrr, Nlim, iFail)
           WRR(kf,k) = rate * Dene  

           prob= zero       ! simple Sum instead of d01 which is bad for multy-spike "SpInEff"
           do iv = 2, nvM    
             eeV = hvV(iv) - BEk  ! energy of recombining free electron
             if(eeV.gt.0.) then 
                V= 5.930968d7*sqrt(eeV)
                Sca= 5040.357* hvV(iv)**3                      ! Planck scale 2*hv^3/c^2; W/cm2/sr/eV
                fun= V*EED(eeV)*SigRR(eeV)*SpInEff(La,iv)/Sca  ! Mihalas (74.2),(74.4)
                dhv = hvV(iv) - hvV(iv-1)	        
                prob= prob+ fun *dhv                           ! cc/s= (cm/s)*(1/eV)*(cm^2)*eV
             endif
           enddo
           WiRR(kf,k) = prob *Dene   ! 1/s
        enddo
      enddo

*  Excitation Probability matrix  WEX(k,kf) & backward De-excitation WDX(kf,k).  Notations: (j,k=Lower) --> (j, kf>k)  
      do j = FSS(nX), HSS(nX)
        do k = nuGS(j,nX), nuGS(j+1,nX) -1    ! Lower level is regular QS of "j"; 
          if(BE(k) .LT. 1.d-3)  cycle         ! skip dead lower; leave WEX(k,kf)=0
          do kf= k+1,   NST(nX)               ! thru QSs above "k", part of them have another ion charge  
            if(mthdEX(k,kf,nX).eq.-7) cycle   ! "-7" means that transition not described in "ExcXE...inp":  leave WEX=0
            if(kf.LT.Nnu(nX) .AND.            ! true QS has BE 
     +         BE(kf).LT.1.d-3)    cycle      ! skip dead candidate for Upper; leave WEX(k,kf)=0
            DE= E(kf,nX) - E(k,nX)            ! as both belong to same "j", thus have common GrSt    
            if(DE.le.0.) STOP 'DE<=0 in Ex1'  ! cannot happen because we check & correct "QSsXE...inp" in "Intro" subr. Later distorsion never observed.
            if(DE.GE.up) cycle                ! gap too large for Te(ti)
	      Ifail= -1
            rate= D01ahf(DE, up, tol, npts, rer, FVSEx, Nlim, Ifail)
            WEX(k,kf)= rate * Dene 
            rate= D01ahf(1.d-7, up, tol, npts, rer, FVSDx, Nlim, Ifail)
            WDX(kf,k)= rate * Dene 
          enddo
        enddo
        if(nuAS(j,nX) .GT. 0) then
          do k = kAI1(j,nX), kAI2(j,nX)-1  ! Lower level is AI QS of "j"; the last AIQS cannot be excited
            do kf= k+1, kAI2(j,nX)         ! Upper level must be AI(j) also; Note: (i) AIQSs are treated as , 
c                                              "never-dead", (ii) they have no true BE, they carry initial BE= -13
              if(mthdEX(k,kf,nX).eq.-7) cycle  ! "-7" means that transition not described in "ExcitXE.inp":  leave WEX=0
              DE= E(kf,nX) - E(k,nX)           ! as both belong to same "j", thus have common GS 
              if(DE.GE.up)  cycle
	        Ifail=- 1
              rate= D01ahf(DE, up, tol, npts, rer, FVSEx, Nlim, Ifail)
              WEX(k,kf)= rate * Dene 
              rate= D01ahf(1.d-7, up, tol, npts,rer, FVSDx, Nlim, Ifail)
              WDX(kf,k)= rate * Dene 
            enddo
          enddo
        endif
      enddo



* Dielectronic Capture: (j,k1=reg) + e --> (j-1,kAI).  We follow Griem3 (6.35), i.e. derive WDC from detailed balance with WAiz in high-density limit.
c                                                      Algorithm, used for "Ejm" & "WDC", is checked by exact (in LTE) Griem3 (6.34) formula  
c                                                            WDC(k1,kAI) = WAiz(kAI,k1)*popLTE(kAI)/popLTE(k1).  
c                                       tCR comparison showed "==" in all POPs (but at that time we used "PI", not "PIR" for Ejm).
      do j = FSS(nX)+1, HSS(nX)       ! capturing ion       
        if(nuAS(j-1,nX).lt.1) goto 9       ! no AI QSs in SS=j-1          
        do k1 = nuGS(j,nX), nuGS(j+1,nX)-1    ! candidates for capturing QS, they are regular QSs
          if(BE(k1) .LT. 1.d-3)  cycle        ! dead QS
          do kAI= kAI1(j-1,nX), kAI2(j-1,nX)  ! AI QSs of SS=j-1.  Note: (i) AIQSs are treated as "never-dead", 
c                                                                        (ii) they have no true BE, they carry initial BE= -1		 
            if(WAiz(kAI,k1,nX) .gt. zero) then       ! this (kAI,k1) couple has DC/AI channel
              Ejm= E(kAI,nX) -PIR(j-1,nX) -E(k1,nX)  ! General case. Griem3 "Ejm" in p.178.
              wFac= 4.*(0.88d-16*13.6/Te)**1.5
              rate= wFac* (g0(kAI,nX)/g0(k1,nX)) 
     +            * WAiz(kAI,k1,nX) *dexp(-Ejm/Te)   ! (6.35)
              WDC(k1,kAI) = Dene * rate              ! (6.34) 
            endif
          enddo     ! kAI
        enddo       ! k1 
  9     continue
      enddo         ! j
      return
      END    ! of 'Ws' subr



      real(8) function EED(eeV)   ! Electron energy distribution. 
	use mo1                     ! Note: bp=0 results in Maxwellian
      implicit none
	real(8) eeV, EED2, EED1 
      EED2= zero
      if((bc-bw/2.) .lt. eeV .and. eeV .lt. (bc+bw/2.))
     +	           EED2= 1./bw
      EED1= 2.*sqrt(eeV/pin/Te)*exp(-eeV/Te)/Te    ! LL StatPhys p 108  
      EED= (one- bp)*EED1 + bp*EED2
      END


      real(8) function FVSinz(eeV)
	use mo1
	implicit none
	real(8) eeV, V, EED, SigInz       
         V= 5.930968d7*sqrt(eeV)          ! cm/s,  sqrt(2kE/m)
         FVSinz= EED(eeV)*V*SigInz(eeV) 
      END           
	

      real(8) function SigInz(eeV)     !  (SS=j, k) + e = (j+1, kf) +2e
	use mo1
	implicit none
	real(8) eeV, xw, yw, OM, Co, Uv  ! OM is MFGu Collision Strength (Omega)
      SigInz= zero
      if(eeV.lt.BEk) STOP 'Came in SigInz with E < BEk'

      if(k.le.LastAI(nX)) then  !  regular & AI QSs are provided with 4 coefs to FAC formula for SigInz(x); obtained from "InzXE....inp" files
        xw= eeV/BEk
        yw= one - one/xw  ! MF Gu 
        OM = Aix(k,kf,nX)*Dlog(xw)+ Bix(k,kf,nX)*yw*yw +   ! FAC guide (2.9)
     +       Cix(k,kf,nX)*yw/xw   + Dix(k,kf,nX)*yw/xw/xw
        SigInz= 3.8101e-16* OM /eeV /g0(k,nX)              ! FAC guide (2.10): "in A.U. e-imp SigInz= OM/k0^2/g(k)". 
c                                               For A.U. see Radcig p13. In particular, A.U. [Sig]= a0^2=2.8003(-17)cm2. 
c                                               Sampson (former EXCIT "case 6") has SigEx= pi*a0^2*Ry* OM/eeV/g0(k,nX)== 1.2(-15)*....  
c                                               Note that 1.2(-15) is x3.14 nore than 3.81(-16) just what MFGu explains: "the missing 
c                                               factor 3.14 as compared to excitation is due to the difference in normalization...".
      else               ! 'k' is RY QSs
        Co= 8.8d-17      !  pi*ao^2, cm^2;  Bohr radius a0= 0.52917725 (-8) cm
        Uv= eeV/BEk -one !  Vainshein-Shevelko scaled energy;  'BEk' is density-dependent but we believe that lowering of continuum in plasmas 
c                       	        does not ruin classical & quantum E-scaling, which is SigInz(E) ~ F(Uv)/BE^2 , see Palch-Shev (5.2.6);  also ZR(6.83) or Griem3 (6.12).  
c                                 Asymptotics & main features of SigInz(E) for atoms & ions are given in Palch-Shev (5.2.2). When BE --> 0, then SigInz rises ~ 1/BE^2, but E-span of large 'SigInz' (say 10*BE) decreases ~BE.  
        SigInz= Co*(13.6/BEk)**2
     +            *6.47*Uv/(1.04+Uv)/(one+Uv)  ! Shevelko's fit to CBE (ATOM) calcul for direct electron-impact ioniz cross-secs; see Palchikov-Shev, p.158.
      endif                                    ! Two coefs: Csh= 6.47 and Fsh= 1.04 taken for n=5 (as "RY") and l=4 as most-probable of l= 0,..., n-1 (~g0)
c                                              ! Actually, the coefs are not sensitive to 'l' for large p.q.n., see table in Pal'chikov-Shev, p. 159
      if(SigInz .lt. zero)   SigInz= zero      ! Bad fits to FAC points may have Sig < 0 even far from threshold 

c     rPSI= 0.53e-8*nprin(k,nX)**2/kiSS(k,nX)  ! mean radial spread of hydrogenic "PSI^2" [Cowan, p.73] SIMPLIFIED by l= n-1, as no "lorb" more
c     rCRO= min(distII, 2.*rPSI)               ! radial spread of cross-sec: smaller of two, see comments in the AKM DayBook
c     SigMax= 3.14*rCRO**2                     ! Upper limit on e-impact cross-sec. It reduces the effect of bad fits in "InzXE...inp".
c     if(SigInz .gt. SigMax) SigInz= SigMax    ! In particular, it reduced sat-to-line ratios to reasonable values. 
      END



      real(8) function FVStbr(eeV)   ! In ionization we used Notations: (j,k) + e = (j+1,kf) +2e 
	use mo1
	implicit none                  ! Our "Non-Maxw EED" is Maxw+Beam. High-E electrons do not recombine, therefore 'recombining el density'= Dene*(1-bp)
	real(8) eeV, SigTbr, V, EED, SigInz
      SigTbr= Sah* SigInz(eeV+BEk)*(eeV+BEk)*g0(k,nX)/g0(kf,nX)/eeV  ! demanding detailed balance in partial (Maxw-Bolt-Saha) LTE; tCRC pap
      V = 5.930968d7*sqrt(eeV)  
      FVStbr= EED(eeV) * V * SigTbr   
      END     


      real(8) function FVSrr(eeV)
      implicit none
      real(8) eeV, V, EED, SigRR       
      V= 5.930968d7*sqrt(eeV)
      FVSrr = EED(eeV) *V *SigRR(eeV)
      END           


      real(8) function SigRR(eeV)   ! Notations in ionization where (SS=j, k) + hv --> (SS=j+1, kf) + e   
      use mo1
      implicit none
      integer ksave, kfsave
      real(8) eeV, hv, SigPhI

      hv= eeV + BEk
      SigRR= SigPhI(hv)*g0(k,nX)*hv**2/g0(kf,nX)/eeV/1.0220d6   ! BlackFold (39); 1.022 MeV= 2mc^2 

      if(ReverseKKF .GT.1.) then  ! In FB Emission notations are (SS=j+1,k) + e  --> (j, kf) + hv
c                                 Therefore for correct computation of "SigRR" I must reverse k,kf   	 
         ksave = k   
         kfsave= kf
         k = kf         ! like-in-ioniz notations (j,k) + hv --> (j+1,kf) for calling func "SigRR" 
         kf= ksave 
         SigRR= SigPhI(hv)*g0(k,nX)*hv**2/g0(kf,nX)/eeV/1.0220d6 
         k = ksave      ! returm sense of k,kf in FB emi              
         kf= kfsave
         ReverseKKF = 0.
      endif

      if(SigRR.lt.zero) then 
        write(*,'(a33, i2, i3, 2i4, 3f9.2, e10.2)')
     +   'SigRR < 0 for XE, j, k, kf, BEk, hv, eeV, SigPhi =', 
     +                  nX, j, k, kf, BEk, hv, eeV, SigPhI(hv) 
        STOP '  2025'
      endif    
      END               


 		
	real(8) function SigPhi(hv)  ! cm2, eV;  PhotoIonz Cross-Sec.   Notations:  (j,k) + hv --> (j+1,kf) + e
      use mo1
	implicit none
	real(8) hv, xw
	SigPhi= zero
  	if(hv .lt. BEk) STOP 'Came in SigPhi with hv < BEk'
      xw = hv/BEk
      SigPhi= (Eix(k,kf,nX)+ Fix(k,kf,nX)/xw + Gix(k,kf,nX)/xw/xw) / 
     +                                         xw**(3.5+Hix(k,kf,nX))   ! MFGu fit to SigPhi(x), see "GU expres for Cross-sec.pdf" file from Bern  
      if(SigPhi.lt.zero) SigPhi= zero          ! Bad fits to FAC points may have Sig < 0 even far from threshold 

c     rPSI= 0.53e-8*nprin(k,nX)**2/kiSS(k,nX)  ! mean radial spread of hydrogenic "PSI^2" [Cowan, p.73] SIMPLIFIED by l= n-1, as no "lorb" more
c     rCRO= min(distII, 2.*rPSI)               ! radial spread of cross-sec: smaller of two, see comments in the AKM DayBook
c     SigMax= 3.14*rCRO**2                     ! Upper limit on e-impact cross-sec. It reduces the effect of bad fits in "InzXE...inp".
c     if(SigPhi .gt. SigMax) SigPhi= SigMax    ! In particular, it reduced satell-to-line ratios to reasonable values. NOMAD also uses SigMax; therefore, with "SigMax" in AKR, the codes agree better.
      END



      real(8) function FVSEx(ev)
	implicit none
	real(8) ev, V, EED, SigExc       
        V= 5.930968d7*sqrt(ev)
        FVSEx= EED(ev)* V* SigExc(ev)
      END



      real(8) function FVSDx(ev)   ! In notations of excitation: (j,k=Lower) --> (j, kf>k)
	use mo1
	implicit none
	real(8) ev, V, SigDx, SigExc , EED       
      SigDx= SigExc(ev+DE)*(ev+DE)*g0(k,nX)/g0(kf,nX)/ev   ! Klein-Rosseland, SVYu95, p5    
      V = 5.930968d7*sqrt(ev)
      FVSDx = EED(ev) * V * SigDx
      END                          

                                          
      real(8) function SigExc(eeV)  ! 'k' is lower,  'kf' is upper
	use mo1
	implicit none
	integer Me
      real(8) eeV, As, Bs, Cs, Ds, Es, fReg, X, X2, Sig, Gaunt ! , Denom, Fs  
      As= Ax(k,kf,nX)
      Bs= Bx(k,kf,nX)
      Cs= Cx(k,kf,nX)
      Ds= Dx(k,kf,nX)
      Es= Ex(k,kf,nX)
c     Fs= Fx(k,kf,nX)      ! 5th coef not used im methods 5 and 11 
	Me=	MthdEX(k,kf,nX)
      fReg = fVR(k,kf,nX)
      if(DE.LE.zero) then
        write(*,'(a40, 3i5,f15.4)') 'DE <= 0 in SigExc; XE, k, kf, DE=', 
     +                                                  nX, k, kf, DE
        STOP 
      endif 

      X= eeV/DE      
      if(X.LT.1.) then
        write(*,'(a40, 3i5, f9.4)') 'X < 1 in SigExc; XE, k, kf, DE=', 
     +                                                nX, k, kf, DE 
        STOP
      endif      
      X2= X*X

      SELECT CASE (Me)  ! # for fitting formula selection
	Case(-7)          ! true-RY & RY-RY excitations.  All (k,kf) couples ignored by "ExcXE...inp" also have 
c                             Me=-7 BUT they do not come here, as filtered out by "WEX() & WDX() computation loops. 
	  if(fReg.lt.zero) STOP 'No SigExc formula for meth=-7 & fVR<0'
	  if(fReg.eq.zero) then
          write(*,'(a57, 4i4, f9.2)') 
     +       'Came to SigExc with me=-7 & f=0 for SSk, k, SSf, kf, DE=',
     +        kiSS(k,nX), k, kf, DE
          STOP
        endif	  
c       We expect that only true-RY & RY-RY excitations come here; for them fReg > 0 is calculated in "Intro"
        if(j.EQ.1) Gaunt= (.276- .18/X)*Dlog(X)         !  "Dn > 0" transition in atoms
        if(j.GT.1) Gaunt= .349*Dlog(X) +.0988 +.455/X   !  Transition in H-like ions 
        Sig = Ampli* fReg* Gaunt /X/DE/DE               !  Such couple is given the VR cross-sec (4)  

      Case(0)                 ! VR;  "ExcXE...inp" contains "f Regemor" > 0 for this (k,kf) couple.  
        if(fReg.lt.zero) then
	    write(*,'(a35, 4i5)')'Came to VR with fVR < 0; XE, j, k, kf=',
     +                                                   nX, j, k, kf
		STOP
        endif		  
        if(j.EQ.1 .and. nprin(k,nX).EQ.nprin(kf,nX))     ! "Dn = 0" transitions in atoms
     +              Gaunt= (.33-.3/X + .08/X2)*Dlog(X)   !   our Go (4) for atoms
        if(j.EQ.1 .and. nprin(k,nX).NE.nprin(kf,nX))     ! "Dn > 0" transitions in atoms
     +              Gaunt= (.276- .18/X )*Dlog(X)        !   our G> (6) for atoms
        if(j.GT.1 ) Gaunt= .349*Dlog(X)+ .0988+ .455/X   ! Ions. Actually, for "n --> n'" in H-like ions (5) 
        Sig = Ampli *fReg *Gaunt /X/DE/DE  
          
      Case( 5)
        Sig= As/X +Bs/X2 +Cs/X/X2 +Ds/X2/X2 +Es*Dlog(X)/X 
      Case(-5)                                           ! meth = -5 is "meth=5 but low accuracy " Lenya
        Sig= As/X +Bs/X2 +Cs/X/X2 +Ds/X2/X2 +Es*Dlog(X)/X 

      Case(11)                                           ! Dima Proselkov. in CODA SGM=(C+B*X1+A*X12)/(X1+D)**4/X1**E 
	  Sig= (Cs +Bs*X +As*X2)/(X+Ds)**4 /X**Es 
      END SELECT

      if(Sig .lt. zero) Sig= zero              ! Bad fits to FAC points may have Sig < 0 even far from threshold 
c      rPSI= 0.53e-8*nprin(k,nX)**2/kiSS(k,nX)  ! mean radial spread of hydrogenic "PSI^2" [Cowan, p.73] SIMPLIFIED by l= n-1, as no "lorb" more
c      rCRO= min(distII, 2.*rPSI)               ! radial spread of cross-sec: smaller of two, see comments in the AKM DayBook
c      SigMax= 3.14*rCRO**2                     ! Upper limit on e-impact cross-sec. It reduces the effect of bad fits in "ExcXE...inp".
c      if(Sig .gt. SigMax) Sig= SigMax	         ! In particular, it reduced satell-to-line ratios to reasonable values. 
      SigExc= Sig                              ! NOMAD also uses SigMax; therefore, with "SigMax" in AKR, the codes agree better.
      END



      SUBROUTINE POPdot(tSC, wPOP, dPOPpdt)
	use mo1
	implicit none
	real(8) tSC, wPOP(NSTm), dPOPpdt(NSTm), sum, xx
      do k = 1, NST(nX)
        sum = zero
        do k1 = 1, NST(nX)              ! For k1=k the PM(k,k)= -Wout(k,nX,La)
          sum = sum+ wPOP(k1)*PM(k1,k)  ! for RateEq dPOP(k)/dt= sum(k) 
        enddo
        dPOPpdt(k)= sum/Scale           ! for slow-time d02
      enddo
      xx= tSC*zero   ! to avoid "tSC not used" warning
      Return
      END


    
      SUBROUTINE ConvoTX(Simul, Co)
      use mo1
      implicit none
      integer imax, imin, iw, npf, kfg, ix
      real(8) Simul(nvM), Co(nvM), v0, FWin, Sver, GaPa, FuIns,
     +                    dom, hvD1, hvD2, domR, dhvf, part, Simuf, hvF 
	Co= zero
* Gauss shape: p(v)dv= [1/sqrt(pi)]*exp(-[{v-v0}/Dop]^2)*(dv/Dop); FWHM= 2*sqrt[ln(2)]*Dop= 1.66510922*Dop;  Vains, p.248; Griem3 p.54

	do iv= iTX1+1, iTX2-1         ! ignore edge points: for them convo is 1/2 of true
        v0  = hvV(iv)
c       BroIns= 1726.  - 0.585*v0   ! YM, Aug 7, 2009: Bro= 920 at 9A (==1378eV) and Bro= 690 at 7A (==1771eV). Let it go linearly thru these 2 points 
        BroIns= 1634.5 - 0.518*v0   ! Sandia-2018 linear fit, see their "TREX_Spectral_resolution_EovdE.png" 
        if(BroIns .gt. 1000.) BroIns= 1000.  
        if(BroIns .lt.  650.) BroIns=  650.  
	  FWin= v0/BroIns             ! eV; FWHM of Instrumental func;   BroIns== v0/{FWHM-caused-by-instr}
	  Sver= zero                  ! Svertka for v0
        GaPa= FWin/1.66510922d0     ! GAuss PArameter "GaPa" is "Dop"== FWHM/1.66510922
        do iw= iTX1, iv
	    if(hvV(iw).gt. v0-5.*GaPa) goto 1  ! Gaussian of hvV(v0-5.*GaPa) is about exp(-25)  
	  enddo
  1     imin= max(iTX1+1, iw-1)     ! the smallest v-point# to be taken for the convolusion 
        do iw= iv, iTX2 
	    if(hvV(iw).gt. v0+5.*GaPa) goto 2  ! this point is too far from v0. It's contrib to "Convo(v0)" is about ~exp(-25) 
	  enddo
  2     imax= min(iw, iTX2)              ! for last "iw" condition may be not satisf, then iw= iTX2+1 after the loop                    
        if((imax-imin).le.12) goto 3     ! not enaugh v-points for accurate convo: go & build FINE grid
        do iw= imin, imax                ! ## of hv' points for integr{...dv') == convolution over v'
	    dhv= (hvV(iw+1)- hvV(iw-1))/2.
          if(dhv.gt.GaPa/10.) goto 3                     ! not enaugh v-points for accurate convo: go & build FINE grid
	    FuIns= exp(-((hvV(iw)-v0)/GaPa)**2)/GaPa/sqpi  ! Gaussian InstrumFunc== Gaus of known FWHM (YM)
          Sver = Sver + Simul(iw) *FuIns *dhv
	  enddo
        goto 9   ! on hvV-grid, finished the convolution for this v0
  3     Sver= zero        ! change the hvV-grid to fine one 
        dom = 10.*GaPa                     ! max size
        hvD1= max(hvV(imin), v0-5.*GaPa)   ! left edge
        hvD2= min(hvV(imax), v0+5.*GaPa)   ! right edge
        domR= hvD2 - hvD1                  ! revised          
        npf = domR/(GaPa/10.)      ! number of points on the FINE v-grid in "domR"
        dhvf= domR/npf             ! = GaPa/10.  
        do kfg= 1, npf             ! convolution on the fine grid
          hvF= hvD1 + dhvf*(kfg-0.5)                  ! points on the fine grid 
          FuIns= exp(-((hvF-v0)/GaPa)**2)/GaPa/sqpi   ! Gaussian Instrum Func to show contrib of hvF to Conv(v0)
	    do ix= imin-1, imax                               ! find left & right to hvF points of hvV grid 
             if(hvV(ix).le.hvF .and. hvF.le.hvV(ix+1)) goto 4 
          enddo
          if(ix.eq.imax+1) then
            write(* ,'(2(i7,f11.4), e14.7, i7, f11.4)') iv, v0, 
     +         			   imin, hvV(imin), hvF, imax, hvV(imax)
            write(41,'(//a30)') 'DID NOT FIND POINT.  Info:'
            write(41,'(2(i7,f11.4), e14.7, i7, f11.4)') iv, v0, 
     +        			   imin, hvV(imin), hvF, imax, hvV(imax)
            STOP 'did not find the point'
          endif
  4       part= (hvF- hvV(ix))/(hvV(ix+1)-hvV(ix))
          Simuf= Simul(ix)+ (Simul(ix+1)-Simul(ix))*part								            
          Sver = Sver + Simuf *FuIns *dhvf
        enddo
  9     Co(iv)= Sver 
      enddo
      Return
      END     ! of 'Convolve' subr  



      SUBROUTINE FindFWHM(Funk, ItsFWHM)  ! Find FWHM of function "Funk" 
      use mo1                             ! Here applied to Mg LyB only 
      implicit none
      integer i1, ivTop, ivHHr
      real(8) Funk(nvM), ItsFWHM, HaHe 

c  FIND the TOP of "Funk",  JV-shape has 2 equivalent peaks. Find the top of softer peak
      do i1 = iTX1, iTX2  
         if(hvV(i1) .LE. (1744.75 -12.)) cycle ! Mg LyB hvC = 1744.75 eV
         if(hvV(i1) .GT. (1744.75 +12.)) cycle 
         if(Funk(i1+1) .GE. Funk(i1)) then     ! Note: no pedestal, we started from pure Stark shape of Mg LyB         
            ivTop = i1+1                            		   
            hvTop = hvV(ivTop)
	      Top   = Funk(ivTop) 
            if(Funk(ivTop+1) .LT. Funk(ivTop)) goto 1  ! the curve went down
         endif		     
      enddo
  1   HaHe = Top/2.  ! Half-Height of the TOP  
c      write(40,'(/f9.3,3f12.6)') hvTop, Funk(ivTop-1), Top,Funk(ivTop+1) 

c  FIND "hvRed" that is the red edge of FWHM
      do i1 = iTX1, iTX2  
         if(hvV(i1) .LE. (1744.75 -12.)) cycle ! Mg LyB hvC = 1744.75 eV
         if(hvV(i1) .GT. (1744.75 +12.)) cycle 
         if(Funk(i1) .LE. HaHe .and. HaHe .LT. Funk(i1+1)) then   
            ivHHr= i1	          
            Frac = (HaHe-Funk(i1)) / (Funk(i1+1)-Funk(i1))      
            hvRed = hvV(i1) + Frac * ( hvV(i1+1)-hvV(i1) ) 	 
            goto 2
         endif		     
      enddo
  2	ItsFWHM = 2.*(1744.75 - hvRed)
c      write(40,'(3f12.6)') Funk(ivHHr), HaHe,  Funk(ivHHr+1) 
c      write(40,'(4f12.3)')  hvV(ivHHr), hvRed,  hvV(ivHHr+1), ItsFWHM 
      Return
      END     ! of subr 'FindFWHM'  



      SUBROUTINE GauConvo(Simul, Co, Bro, CM)  ! Convolution with Gaussian function is CALLed 
c                                                in 3 cases as follows. 
c          First,  convo with instrumental func; then 
c                  (a) Convo Mode CM=1, (b) "Bro" deliveres "hv/FWHM of Gaus", say, BroIns= 900. 

c          Second, for convo FBemi & BFabso with Gau of width DEw [eV], see introductory comment; then
c                  (a) Convo Mode CM=2, (b) "Bro" == DE.
     
c 	     Third,  for convo FBBF edges with TiD-Gau: 
c                  (a) Convo Mode CM=3, (b) "Bro" is FWHM/hv0 like in thermal Doppler bro ~ 0.001
      use mo1 
      implicit none
      integer i1, iw,  CM 
      real(8) Simul(nvM), Co(nvM), v0, FWin, Sver, dev, Bro,   
     +                    FuIns, Gauss, prF   !  , InsBro 
      Co= Simul       ! At least. It will remain for non-Convo hv-domains, see below 
      do i1= 1, nvM			
        v0 = hvV(i1)
        if(v0.lt.      hvSmo ) cycle ! I excluded lines at hv < hvSmo because (i) here BB << FF, (ii) they overlap, thus their shape cannot be resolved.
        if(v0.gt. 0.99*hvMaF ) cycle ! Because at large hv: dist betw v-points is > Bro = DEw 

c	  introduce "FWin", which is FWHM [eV] of Gaussian to be used for Gau-convolution
c       if(CM.eq.1) Bro= InsBro(v0)  ! OPTION for inclu v-depe instead of from-FLAG constant, like needed for Z2011  
        if(CM.eq.1) FWin= v0/Bro     ! eV;  by definition, "Bro" = "hv/FWHM of Gaus", say, instrumental 900 
        if(CM.eq.2) FWin= Bro        !      by definition, "Bro" is FWHM of the Gaussian
        if(CM.eq.3) FWin= v0*Bro     !      by definition, "Bro" is FWHM/hv0 like in thermal Doppler bro ~ 0.001

        Sver= zero    ! Svertka for v0
        prF = zero
        do iw= 2, nvM-1                ! loop over hv' points for integral {...dv'): convolution over v'
          dhv= hvV(iw)- hvV(iw-1)
          dev= abs(hvV(iw) - v0)       ! hv' - hv
          if(dev .gt. 2.5*FWin) cycle  ! this point is too far from v0:  exp[-(dev/[FWHM/1.6651])^2] is too small [ < 4d-8]
          FuIns= Gauss(FWin, dev)      ! Gaussian of known FWHM (former "instrumental function")
          Sver = Sver+ (prF+ Simul(iw)*FuIns)*dhv/two
          prF  = Simul(iw) *FuIns 
        enddo
        Co(i1)= Sver 
      enddo   ! i1
      Return
      END     ! of 'GauConvo' subr 


 
      real(8) FUNCTION Voigt2(fwhmL, fwhmG, vC, dv) ! [eV], dv= v-vC, Sasha's connector to JQSRT Drayson's VOIGT(X,Y); WELL checked
	use mo1                                       
	implicit none
	real(8) fwhmL, fwhmG, vC, dv, balf, x1, y1, Voi2, VOIGT
      balf= (vC/fwhmG)*1.6651092   ! 1.6651092= 2*Sq[ln(2)]
      x1= balf*abs(dv)/vC	         ! to Dr' units
      y1= 1.665109*fwhmL/2./fwhmG  ! to Dr' units
      Voi2= VOIGT(x1,y1)/1.772454  ! with renorm from "sqrt(pi)=1.7724539; Drayson" to 1 
      Voigt2= Voi2*balf/vC	     ! to "eV"
      END


      real(8) FUNCTION VOIGT(X,Y) ! Voigt shape normalized to sqrt(pi); From Drayson, JQSRT, v.16, pp.611-614, 1976
      use mo1
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
      use mo1                            ! has FWHM= 2*sqrt[ln(2)]*GaPa= 1.66510922*GaPa;  Vains, p.248; Griem3 p.54
      implicit none                      ! where GAuss PArameter "GaPa" == FWHM/1.66510922
      real(8) fwG, devW, GaPa 
      Gauss= zero
      GaPa = fwG/1.66510922d0
      Gauss= dexp(-(devW/GaPa)**2)/GaPa/sqpi  ! Gaussian of known FWHM
      END



      subroutine PMg0()  ! For g0 case: transition probability matrix PM(k,kf) and QS depopulation rate Wout(k,nX,La)
      use mo1
      implicit none
c  Total depletion rate of state 'k' into all other states 'kf' is "Wout(k,nX,La)"
      do k = 1, NST(nX)
        Wout(k,nX,La)	= zero     ! depletion rate of QS# k
        if(k .LT. Nnu(nX) .AND.       ! k is regular QS  
     +     BE(k) .lt. 1.d-3)   cycle  ! k is dead, skip it. Remember that nucl & AIQSs are "never dead" 
        do kf = 1, NST(nX)            ! to all acceptors
	    if(kf .LT. Nnu(nX) .AND.       ! regular, not nucl
     +       BE(kf) .lt. 1.d-3)  cycle   ! skip dead acceptor; only regular QSs may be dead.
	    if(kf .ne. k)  Wout(k,nX,La) = Wout(k,nX,La) +    A(k,kf,nX) + 
     2             WInd(k,kf) + Wab(k,kf) + WiRR(k,kf) +   WI(k,kf)	   ! Probabs are k --> kf
     3           +  WMI(k,kf) + WTB(k,kf) + WPhI(k,kf) +  WRR(k,kf) 
     4           +  WEX(k,kf) + WDX(k,kf) +  WDC(k,kf) + WAiz(k,kf,nX)	   
        enddo
      enddo

c  Probability matrix PM(k,kf) is a sum over ALL k --> kf channels
      PM= zero
      do k = 1, NST(nX)
	  if(k .LT. Nnu(nX) .AND. BE(k).LT. 1.d-3) cycle  ! "k" is regular, "k" is dead';  BE(nucl) ==0.002 eV given in Intro 
c                                                       Note: only regular QSs may be dead. Nucl & AIQSs are "never dead" 
        do kf = 1, NST(nX)
	    if(kf .LT. Nnu(nX) .AND. BE(kf).LT. 1.d-3) cycle  ! regular "kf", dead "kf", skip dead acceptor; .
	    if(kf .eq. k)  PM(k,k)  = -Wout(k,nX,La)
		if(kf .ne. k)  PM(k,kf) =     A(k,kf,nX)     +  
     +               WI(k,kf) +  WMI(k,kf) + WTB(k,kf) + WPhI(k,kf) 
     +            + WRR(k,kf) +  WEX(k,kf) + WDX(k,kf) + WAiz(k,kf,nX)
     +            + WDC(k,kf)	+ WInd(k,kf) + Wab(k,kf) + WiRR(k,kf)
        enddo
	enddo
      return
      end subroutine  ! PMg0()




      SUBROUTINE SCENARIO(ngw) ! This Subr gives this-La  R, Te, TiD, Den(nXE), %Mg, bp, bc, bw.   
      use mo1                  ! spheric plasma zone of interest is mentioned as BS (bright source), La=2, "La" is the  namber of a zone               	.	   
      implicit none     
      integer ngw      ! "ng" (here "ngw") is the NUMBER of "tf"-point in t-grid
*                           tf = tgp(ng);  gpEq(j) is # of t-grid point at which tf=tres(j); j= 1,2,3...6 
      real(8) dev_t, HWL  ! abs(t-peak time of Te(t))
	     
      expPCD40 = 0.26d12 ! W; in expt 1520 at -0.06 ns, see Report Fig.7, PCD with 40mils Kapton filter
      expPCD10 = 2.30d12 ! W  in expt 1520 at -0.06 ns, see Report Fig.7, PCD with 10mils Kapton filter
      expPCDBe = 22.3d12 ! W  in expt 1520 at -0.06 ns, see Report Fig.7, PCD with PCD with Be filter
      expXRD   = 74.2d12 ! W  in expt 1520 at -0.06 ns, see Report Fig.8  XRD 

      MgLyAmin = 4.9  ! eV, see Report, formula (4) , expt data from shot 1520
      MgLyAmax = 5.3  ! eV, see Report, formula (4) 
      MgLyBmin = 6.5  ! eV, see Report, formula (5) 
      MgLyBmax = 10.  ! eV, see Report, formula (5) 

*  Radii (cm)
      R1= 1.d-9      ! cm, outer radius of "hole" that is small zone (La=1) in the center of BS 
      R3= R2+ 1.d-9  !      warm plasma (La=3) around BS (La=2)

*  Parameters [R, Te, TD, ni, part of Mg in total ni] in spheric BS (La=2) 
      if(La.eq.2) then    

        if(ngw .LE. gpEq(1)) then	! i.e. up to t of 1st "print frame"	  
           frac = (tgp(ngw)-tgp(1))/(tgp(gpEq(1)) -tgp(1))
           R2   = ( Dstrt + frac* ( Dfr(1) - Dstrt ))/2.     ! [cm]  Outer radius of BS, (La=2)  
           Te   =  TeStrt + frac* (TeFr(1) - TeStrt)         ! [eV]  electron temperature in La=2
           TiD  = (TDstrt + frac* (TDfr(1) - TDstrt))*1000.  ! [eV]  Doppler temperature in BS
           DenI =   nStrt + frac* ( nFr(1) - nStrt )         ! [i/cc]  AL+MG
           paMG = (peMGst + frac* (peMG(1) - peMGst))/100.   ! part of Mg ions in Mg+Al mixture, i.e. in "DenI"
           goto 5
        endif

        do i= 1, mSpe-1    
          if(ngw.GT.gpEq(i) .and. ngw.LE.gpEq(i+1)) then  
            frac = (tgp(ngw) - tgp(gpEq(i))) /
     +             (tgp(gpEq(i+1)) -tgp(gpEq(i))) 
            R2   = ( Dfr(i) + frac* ( Dfr(i+1) - Dfr(i)))/2.    ! [cm].  
            Te   =  TeFr(i) + frac* (TeFr(i+1) - TeFr(i))       ! [eV]
            TiD  = (TDfr(i) + frac* (TDfr(i+1) - TDfr(i)))*1000 ! [keV] to [eV]
            DenI =   nFr(i) + frac* ( nFr(i+1) -  nFr(i))       ! [i/cc]  AL+MG
            paMG = (peMG(i) + frac* (peMG(i+1) - peMG(i)))/100. ! from % to "part of DenI"
            goto 5
          endif 
        enddo

        if( ngw.GT.gpEq(mSpe)) then   ! after last PrintFrame time 
            R2   =  Dfr(mSpe)/2.      ! [keV] to [eV]
            DenI =  nFr(mSpe)         ! [i/cc]  AL+MG
            paMG = peMG(mSpe)/100.    ! from % to "part of DenI"
            goto 5
        endif
  5     continue
  
        if(KeySmooth .GT. 1) then  ! Replace broken-line t-dependence of Te(t)s with smooth Lorentzian type t-dependence 
c                                  to avoid discontinuities in dTe/dt which cause jumps in Ti(t) via dTe/dt term in eq. (3).  	   
c                                               "ngw" is the point# on t-grid; 
c                                               "gpEq(j)" is t-grid point# at which t = tres(j)  	   		          
                                     HWL = HWL1    ! [s] HWHM of before-peak Te(t)
           if(ngw.GT.gpEq(tresPeak)) HWL = HWL2    ! [s] HWHM of  after-peak Te(t)
           dev_t = tgp(ngw) - tgp(gpEq(tresPeak))  ! [s] delta-t from the time of Te-peak. 
           Te = PeakTe/(1.+ (dev_t/HWL)**2)  ! [eV]
        endif

        uLOS(2)= 0.d0  ! [cm/s] velocity of BS along a LOS, not used in this version of the code      

        TeBS = Te
        Volu(2)= 1.3333*pin* R2**3    ! [cm3] Volume of spherical BS 
 
        Den(1)= DenI*(1.-paMG)  ! (ions ofAL)/cc; Here "1" means nXE # 1 that is AL, 
        Den(2)= DenI*paMG       ! for "paMG= 0.0552" see  "****" 3 lines sbove 

c  Note: 5% of mass in the alloy results in 5.844% in ion number because
c            mass of Mg in the mixture "Mmg" = 0.05 (Mal+Mmg) can be written as 
c                                  24.30*nMG = 0.05 [24.30*nMG +26.98*nAL)
c           thus 26.98*nAL = 20*24.30*nMG -24.3*nMG = 19.*24.30*nMG , 
c           thus nMG = nAL * 26.98/(19*24.30) = 0.05844 * nAL.
c                nAL+nMG = DenI thus [nAL + 0.05844*nAL = DenI] 
c           thus nAL= DanI/1.05844 = DenI * 0.9448; 
c           thus nMG= DenI - nAL = DenI * 0.0552                                 

        MeanM = Imas(1)*(1.-paMG) + Imas(2)*paMG   ! [g]. not A.U.
        MaBSvsMw(2)= Volu(2)* DenI* MeanM /3.e-3   ! vs total wire array mass=3mg
        u3D = sqrt(8.*BolEr*TiD/pin/MeanM)         ! here thermal + hydro
      endif ! "if La=2"       

      distII= one/DenI**third  ! cm; mean ion-ion distance
      ZC1 = zero               ! mean ion charge   in the layer: average over XE & SS (including atoms)
      ZC2 = zero               ! mean ion charge^2 in the layer: average over XE & SS (including atoms)
      Dene= zero               ! e/cc in the Layer
      do n7= 1, nXE
        ZC(n7)= zero
      do  k= 1, NST(n7)
        ZC(n7)= ZC(n7)+                POPt(k,n7,La)*(kiSS(k,n7)-1)      ! Note: these POPs are from previous t-step 
        ZC1   = ZC1   + (Den(n7)/DenI)*POPt(k,n7,La)*(kiSS(k,n7)-1)
        ZC2   = ZC2   + (Den(n7)/DenI)*POPt(k,n7,La)*(kiSS(k,n7)-1)**2
        Dene  = Dene  +  Den(n7)      *POPt(k,n7,La)*(kiSS(k,n7)-1)
      enddo
      enddo
      Zbar(La)= ZC1	
      bp= zero      ! beam part of the EED
      bc= 12.d3     ! [eV] central energy of e-beam, 
Check: max energy of the beam, (bc+bw/2) must be within the hv grid, i.e. < hvMax.
      bw= bc/10.d0  ! e-beam width , eV
      Return
      END	
	
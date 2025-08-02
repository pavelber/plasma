      use mo1
      implicit none

C     Tell the compiler that EmiAbso is defined in another file
      EXTERNAL EmiAbso, EffSpInK13,OpenFiles, Intro

      CALL OpenFiles() ! Open output files and write title lines
      CALL Intro()     ! Read input files; fill in atomic data arrays and initial POPs
      CALL LineList()  ! Find spectral lines possible in [hvSmo, hvmax] domain and print LineList

	write(*,'(a50)') 'tf[ns]  D[mm]   Te[eV]  TD[keV]    i/cc     %Mg'  

******************  Build the sequence of t-points
      gpEq= 0               ! integer array of length "mSpe" contains serial numbers of t points at which t = tres(j), j = 1,..., mSpe
      if(npa.LT.4) npa = 4  ! number of t-points in (tres(j), tres(j+1)] t-interval. This number includes t= tres(j+1) point  
      tgp(1) = strt         ! first t-point on the t axis 
      do kw = 2, 50                      ! along t points; kw is serial number of t-point on the t axis
         if(tgp(kw-1) .LT. tres(1)) then 
	      tstep = (tres(1) - strt)/npa  
            tgp(kw) = tgp(kw-1) + tstep     
            Du= (tgp(kw)-tres(1)) / tres(1)      ! deviation  from tres(1)
            if( abs(Du) .LT. 1e-8 ) gpEq(1)= kw  ! "gpEq(1)" is NUMBER of t-point at which t = tres(1)  	
         endif	
         if(gpEq(1) .GT. 0) goto 1   
      enddo
 1    continue

      do j = 1, mSpe-1
         do kw = gpEq(j)+1, gpEq(j)+50       ! along t points; kw is serial number of t-point on the t axis
            if(tgp(kw-1) .LT. tres(j+1)) then 
	         tstep = (tres(j+1) - tres(j))/npa  
               tgp(kw) = tgp(kw-1) + tstep     
               Du= (tgp(kw)-tres(j+1)) /tres(j+1)     ! deviation from  tres(j+1)
               if( abs(Du) .LT. 1.e-8 ) gpEq(j+1)= kw 	
            endif	
            if(gpEq(j+1) .GT. 0) goto 2  
         enddo     ! kw	 
 2       continue
      enddo        ! j

      ng = 1       ! "ng" is serial number of t point; here the first t-point
      tf = tgp(ng) ! first t-point on t axis

********************  Main "ti'- tf" loop

 11   continue    ! do next t-step
      ti = tf     ! "tf" of finished (ti-tf] interval becomes "ti" for present interval.
      ng = ng+1
      tf = tgp(ng)

      La= 2  ! BS, instead of La-loop 	
	
      CALL SCENARIO(ng)  ! for (ti, tf] interval prescribe this-zone (here, this-La)  D, Te, TiD; Den(XE=1), Den(XE=2), bp, bc, bw.      
c                        Note: the RF computation for any zone requires radii of all zones

***   Print governing params [D,Te,TD,ni, part of Mg in denI (<=1)] on the screen. 
***   I show them at the end of (ti-tf] interval because used them for t-integration of POPs from "ti" to "tf". 
      write(*,'(f8.4,  f7.3,    f10.1,   f8.2,    e11.3, f7.2 )')
     +       tf*1.d9, 2*R2*10.,  Te,   TiD/1000., DenI,  paMG*100.  							      

      nX= 0
 15   nX= nX+1 
***                Gaussian FWHM/hvC of spectral lines 
      FWkVcGAU(nX)= 1.66511*sqrt(2.*BolEr*TiD/Imas(nX))/c  ! Doppl FWHM(Ti)/vC = sqrt[ln(2)*8k*Ti/M]/c; see Griem3 p.54 == NRL (25) p.56 == Vain p.248  

      CALL AtKins()  ! Kinetics for this "La" and "nX": find tf-values of POP(kQS) and POPZ(jSS,nX,La); update POPt(k,nX,La) and BEp(k,nX,La)

      if(nX .LT. nXE ) goto 15    ! process next XE

***   Print [R,Te,TD,ni,niAL,niMG,Zbar,ne] in file MHD_BS.dat. 
***   Relate them to the end of (ti-tf] because used for t-integration up to t=tf. 
      write(32,'(f6.4, f6.3,   2f7.0,    f7.2,   2e10.3, 2f8.3, e11.4)')   
     +       tf*1.e9, 20.*R2, Te,TiBS, TiD/1000., Den,     ZC,   Dene    ! [ns] [mm] [keV] [i/cc]; TiD is Doppler temperature [keV]

c  Print all-XE POPZ for this "La" and t = tf   			
      do  nX = 1, nXE
        do jSS = FSS(nX), HSS(nX) +1   ! including nucl
           POPZ(jSS,nX,La)= max(POPZ(jSS,nX,La), 1.d-40)
        enddo
      enddo
      write(117,'(f9.4, 10e11.4, f7.3, f9.1, e11.4, f8.2)') tf*1.e9, 
     +  POPZ(10,1,La), POPZ(11,1,La), POPZ(12,1,La), POPZ(13,1,La),     ! "1" is AL:  Be-, Li-, He-, H-like and nucl 
     +  POPZ(14,1,La), POPZ( 9,2,La), POPZ(10,2,La), POPZ(11,2,La),     ! "2" is Mg:  Be-, Li-, He-, H-like and nucl 
     +  POPZ(12,2,La), POPZ(13,2,La), 20.*R2, TeBS,  DenI, TiD/1000.       

      CALL EmiAbso()   ! update this-La EmTot(La,hv) and AbTot(La,hv)
      CALL EffSpInK13() 
      CALL RTeSpIn()   ! Calculate towards-detector Radiation Flux from BS [W/eV/sr]. 

      do k3= 1, mSpe                ! check all in-FLAG print-times for prints (60+ and (170+ 
         if(ng .eq. gpEq(k3)) then  ! "gpEq(...)" is NUMBER of t-point at which t = tres(...): CALL SUBR "TXscans" 
            CALL TXscans(k3)        ! simulate TREX scans "(60+" and FuFlux "170+" at t-point #k3
         endif 
      enddo

      CALL PowYie()  ! estimate plasma radiation Power(t) and spectral yield (J/eV)
  
      goto 11	 ! Note: the run will be stopped by "if(FrNu .eq. mSpe) STOP 'Finished the Computation.'  
      END ! MAIN




 
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
         CASE ('l')	  ! in 4l AIELs of [He], say neon
           char2int = 0
         CASE DEFAULT
           WRITE (*,*) symbol, ' not convertible into integer'
           STOP 'STOPped in function char2int'
        END SELECT
      END







      real(8) function K13(kR)   !  for uniform spherical plasma
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








      SUBROUTINE PowYie()  ! estimate radiation Power [TW] from 1 BS..  
      use mo1
      implicit none   
      integer it, itL 
      real(8) PwBe, Fpow, FpowPr, Pow5, Pw10, Pw40,   
     +        RespBe, Resp40, Respo5, Resp10        ! Response of four PCDs. 0 <= Respo <=1   
	 
      PwBe  = zero  ! [W]: v-integral of "RadFlux" thru Be-filter in 4pi sr  
      Fpow  = zero  !                 full v-range   
      Pow5  = zero  !                 thru 5 mils Kapton PCD filter
      Pw10  = zero  !                     10
      Pw40  = zero  !                     40

      do iv= 2, nvM 
         do it=1, npBe8-1  ! use the response of PCD with 8mcm Be filter to find the respo for current "iv"  
            if(hv8Be(it).lt.hvV(iv) .and. hvV(iv).le.hv8Be(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv8Be(itL))/(hv8Be(itL+1)-hv8Be(itL))		              		   
         RespBe= RespoBe(itL)+ Frac*(RespoBe(itL+1)-RespoBe(itL))     ! Response of Be-filtered PCD for "iv"; 0 <= Respo <=1   

c  PCD with 5 mils Kapton filter transmission
         do it=1, np5kap-1                                            ! find the response for current "iv"  
            if(hv5kap(it).lt.hvV(iv) .and. hvV(iv).le.hv5kap(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv5kap(itL))/(hv5kap(itL+1)-hv5kap(itL))		              		   
         Respo5= Resp5kap(itL)+ Frac*(Resp5kap(itL+1)-Resp5kap(itL))  ! Response of PCD filterd with 5 mils Kapton, for "iv", 0 <= Respo <=1   

c  PCD with 10mils Kapton filter
         do it=1, np10ka-1                                              
            if(hv10ka(it).lt.hvV(iv) .and. hvV(iv).le.hv10ka(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv10ka(itL))/(hv10ka(itL+1)-hv10ka(itL))		              		   
         Resp10= Resp10ka(itL)+ Frac*(Resp10ka(itL+1)-Resp10ka(itL))  ! Response of PCD filterd with 10mils Kapton, for current "iv"

c  PCD with 40mils Kapton filter
         do it=1, np40ka-1                                            ! find the response at current "iv"  
            if(hv40ka(it).lt.hvV(iv) .and. hvV(iv).le.hv40ka(it+1)) 
     +          itL= it
         enddo  
         Frac= (hvV(iv)-hv40ka(itL))/(hv40ka(itL+1)-hv40ka(itL))		              		   
         Resp40= Resp40ka(itL)+ Frac*(Resp40ka(itL+1)-Resp40ka(itL))  ! Response of PCD filterd with 40mils Kapton, for current "iv"; 0 <= Respo <=1   
         dhv= hvV(iv)-hvV(iv-1)

c    hv-integration. Powers from 1 BS. 
c    RadFlux(iv) is computed as 10-belts sum of BeltArea * ThruBelt Spin(iv), thus [W/eV/sr] from 1 BS;  . 
c    PCD covers solid angle "OmDet" thus collects PowPCD[W] = OmDet[sr]* vInteg{Response*RadFlux}[W/eV/sr];   0 <= Respo <=1 
c    Full (4pi) solid angle is bigger than "OmDet" by factor "4pi/OmDet", thus 
c    full Power[W] = 4pi[sr]* vInteg{Response * RadFlux[W/eV/sr]}

         Fpow  = Fpow + RadFlux(iv) *dhv          *FoPi  ! [W]; Full-spectrum radiated power (XRD). "Fpow" is printed as "XRD" and used in computation of Ti(t), eq.(2) 
         PwBe  = PwBe + RadFlux(iv) *dhv *RespBe  *FoPi    
         Pow5  = Pow5 + RadFlux(iv) *dhv *Respo5  *FoPi         
         Pw10  = Pw10 + RadFlux(iv) *dhv *Resp10  *FoPi      
         Pw40  = Pw40 + RadFlux(iv) *dhv *Resp40  *FoPi         
      enddo  ! iv  

      RadY   = RadY + tstep* (Fpow+FpowPr)/2.  ! Radiation Yield [J] in full spectral range
      FpowPr = Fpow
 
      write(100,'(f7.4, 19e11.4)') tf*1.d9, Fpow/1.d12, PwBe/1.d12,      ! [TW] in full hv range, in 4pi. Same for PCD-Be;  
     +                          Pow5/1.d12, Pw10/1.d12, Pw40/1.d12,      ! [TW] three PCDs with Kapton filters (5, 10, 40mils)  
     +                                         RadY, Te/1.d3, DenI       ! yield [J]

********** Compu Ti via the electron energy eq. Do it here because fresh value of Rad POWER needed *******

c  Coulomb Log: CouL= Log(Pmax/Pmin); we use Spitzer's (5.14) 
c  Note: Braginski (and Golant after him) lost "-Ln(Zi)" in their final expression (or considered H). 
      CouL= 23.46+ 1.5*log(Te)-0.5*log(Dene)-log(ZC1)             ! in Fortran log==ln; log10==lg
      if(Te.GT .40.) CouL= 25.25+ log(Te)-0.5*log(Dene)-log(ZC1)  ! ZC1 is mean ion charge in the cell: average over XE and SS (including atoms), here from previous t-step
      if(CouL.LT.5.) CouL= 5.
c  typical time betw two elastic scatterings of an electron on ions [Braginskii.2.5.e], 
c  i.e. momentum-transfer time is
      teiBr= 3.441d5* Te**1.5 /CouL /Dene /ZC1  ! s, expr (10) in "EnEq" above;  Dene==ni*ZC1
      teiE = teiBr*MeanM/2./elma                ! s, is ei energy transfer time     

      if(tf .gt. strt+ 2.*tstep ) TeDot= (Te - TePrCO)/(tf-ti)  ! [eV/s] 
      TePrCO= Te	

c  3rd term in eq (Ti-Te)/teiE = dTe/dt + 2*Fpow/3/V1/ne/kB  ! [eV/s]; kB==BolJ in [J/eV] 
      TeDotPow  =  2.*Fpow /3./Volu(2)/Dene/BolJ   !  [eV/s] per 1 e

      TiBS = Te + teiE* (TeDot + TeDotPow)  ! Ion temperature [eV]   
      if(TiBS .le. 0.) STOP 'TiBS <= 0'

      Return					  
      END    ! of 'PowYie' subr

       

      SUBROUTINE AtKins() ! Atomic Kinetics for current La, nX.  On entry POP(EL#) given POPt(k,nX,La) from previous t-step (t=ti)
      use mo1             ! on exit POP(EL#) contains the distribution at t=tf and re-writes POPt(k,nX,La) for current La, nX
      implicit none
      real(8)  POP(NSTm), POPti(NSTm)  ! these files are Local in AtKins
      external POPdot     ! subroutine mentioned by name (no argument) in d02

      CALL redPI()        ! compute Ioniz Potentials for {Te(ti-tf),ni(ti-tf)} of MAIN: PIR(SS,nX)= PI(SS,nX)-dPI(SS,nX)
      CALL BEvPr()        ! compute Binding Energy of ELs

      Sah = Dene*(one-bp)/(6.0371288d21*Te**1.5d0)  ! for 3-body recombination

      do k= 1, NST(nX)
        POP(k) = POPt(k,nX,La)   ! shorter name. POPs from previous t-step
      enddo

CUT-off block
      do k= 1, NST(nX)
        if(POP(k).lt.-1.d-12) STOP 'came to cut-off with POP < -1.d-12'  ! NAG d02 with tolD02= 1.d-6 provides |sumPOP - 1| < 1.e-8 but some small POPs may be negative |POP|<1.e-12
        if( k.LT.Nnu(nX) .AND. BE(k).lt. 1.d-3                           ! dead non-AI EL
     +                   .AND. POP(k).gt. zero) then  ! cut it;  Note: (i) AIELs have BE = -13., I keep them "never dead"  
          POP(nuGS(kiSS(k,nX)+1 ,nX)) = 
     +    POP(nuGS(kiSS(k,nX)+1 ,nX)) + POP(k)        !  Add it to GS of next SS
          POP(k)= zero                                !  empty cut EL 
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
      if(ng .eq. gpEq(PrFr)) then  ! "ng" is the NUMBER of "tf"-point on t axis
        write(41,'(//a7, f5.3, a20)') 'tf=', tf*1.d9, 
     +                                   'ns.  POPs after d02'
        do k= 1, NST(nX)   
          if(k.eq.1) write(41,'(/a56)')       'XE  EL#   QSname        E     
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
     +                  .AND.POP(k).GT. 1.d-15) STOP 'Populated dead EL'
	  if(POP(k) .LT.-1.d-12) Write(*,'(a29,e10.2, a17,i2, i4,e14.6)')  ! NAG d02 with tolD02= 1.d-6 provides |sumPOP - 1| < 1.e-8  but some small POPs may be negative |POP|<1.e-12
     +     'found POP < -1.d-12;  POP=', POP(k),                         ! If these negative values are < -1.e12 we send screen info 
     +     'for XE, EL, BE=', nX, k, BE(k)			  
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

c Save 1D arrays POP(k) and BE(k) in 3D arrays for next t-step with this "La" and "XE" 
      do k= 1, NST(nX)
         POPt(k,nX,La)= POP(k)
	    BEp(k,nX,La)= BE(k)
      enddo

      Return
      END   ! of 'AtKins' subr



      SUBROUTINE redPI()  ! gives Ionization Potentials for {Te(ti-tf),ni(ti-tf)} of MAIN.  
	use mo1             ! two options: "table values (no lowering)", "ion-sphere", depending on "KeReDu" in Flag.inp
	implicit none
	real(8) SpheR, DPIsph(HSSm+1)

      if(ng .eq. gpEq(PrFr)) then   ! "ng" in  the NUMBER of "tf"-point in t axis
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



      SUBROUTINE BEvPr()  ! gives Binding Energy of outermost electron (BE)
	use mo1
	implicit none
      do k= 1, NST(nX)  
        BE(k) = -13.d0  ! '-13' remains for AI ELs 
      enddo

      do j= FSS(nX), HSS(nX)  
         do k= nuGS(j,nX), nuGS(j+1,nX)-1   ! non-AI ELs of j
            BE(k)= PIR(j,nX) - E(k,nX)      ! above-PIR ELs get BE < 0
	   enddo
      enddo
      BE(Nnu(nX))= 2.d-3  ! [eV] Nucleus is given Artificial (wrong) small BE > 0 to  avoid its de-POP by "if(BE < 1.d-3)"
      Return
      END     ! of 'BEvPr' subr



      SUBROUTINE Ws()  ! Transition probabilities for Rate Equations.
      use mo1          ! In each W-matrix, first index shows INITIAL EL of the transition.
      implicit none
      integer  lUL, lw, kAI 
      real(8)  pv(nvM), freq, Wave, DEJ, BemHz, Bem, Bab, hvCw,  
     +         AbSpIn, up, rate, prob, Ejm, wFac, D01ahf,  
     +         SigRR, V, fun, SigPhi, eeV, dev, Voigt2,     ! Voigt2 is Sawa's connector to JELRT Viogt; Earlier used "VoigtJS" that is Jenya's Voigt function but is was WRONG 
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

*  Probability of bound-bound INDUCED emission and absorption: Wind(U,L) and Wab(L,U)
      do kL= 1, NST(nX)-1
      do kU= kL+1, NST(nX)
        if(A(kU,kL,nX) .LT. 1.)      goto 2  ! this kU,kL couple does non play: Weak lines and near-soft-edge lines got A==0 in Intro 
        if(kU.LT.Nnu(nX) .AND.               ! non-AI EL,  
     +     BE(kU).LT.1d-3)           goto 2  ! dead; Note: AIELs are treated as "never-dead"; they have no true BE, thus carry initial BE = -13 
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
c  'Wind(U,L)' and 'Wab(L,U)' are  B * integ{dv*p(v)*SpInEff(v)};  'p(v)' is kL-->kU absorption line shape
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
c                                     Thus many lines are formally present but their U is dead and FWevLor(lw)=0 
         FWevGau= hvCw *FWkVcGAU(nX)  ! Gaussian FWHM [eV] of kU-->kL line of XE# = nX
         Wing   = hvCw/2.             ! Line Wing Length [eV]
         ArPv   = zero
         do iv= 1, nvM 
            dev= abs(hvV(iv) - hvCw)
            pv(iv)= zero
            if(hvV(iv).GT. hvCw-Wing .and. 
     +         hvV(iv).LT. hvCw+Wing      ) then 
               pv(iv)= Voigt2(FWevLor(lUL), FWevGau, hvCw, dev)    ! JELRT (Sawa) Voigt is OK to 1% for any L,G, but slower than wrong "VoigtJS"   
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

*  Ionization (removal of ONE electron) probability matrixes WI(k,kf), WPhI(k,kf) and backward WTB(kf,k), WRR(kf,k), WiRR(kf,k) .  
*                                                  
      up= 100.*Te                              ! Notations: (j,k) + e = (j+1,kf) +2e
      if(bp.gt.1.d-7) up= 100.*Te +bc +6.*bw

      do   k  = 1, LastAI(nX)                  
        do kf = 1, LastAI(nX)    
           if(bra(k,kf,nX) .LT. 0.5) cycle  ! no link;  "bra" is 0 or 1 with FAC bases 
           if(k .LT. Nnu(nX) .AND.          ! non-AI EL,  
     +        BE(k).LT.1.d-3)        cycle  ! this EL is dead; Note 1: Nucl given BE= 0.002 eV; Note 2: Nucl and AIELs cannot be dead 
           if(kf.LT. Nnu(nX) .AND.          ! non-AI EL,  
     +        BE(kf).lt.1.d-3)       cycle  ! this EL is dead; leave WI(k,kf)= 0
           j = KiSS(k,nX)   
           BEk= Eth(k,kf,nX)-DPI(j,nX)      ! energy for "(j,k) + e = (j+1,kf) +2e" ionization
           if(BEk .GE. 0.99*up)      cycle  ! "Te" too low, see limits of d01 integral below 

           if(k.GT.Nnu(nX)   .AND.          ! AI EL. Because of big DPI this k=AI --> kf ionization
     +        BEk .LT. zero) BEk= 1.        ! does not require E, but I give formal 1 eV to avoid 0.  
c                                           Note: "BEk < 0" never happens for non-AI k because big DPI cuts it. 
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

*  Excitation Probability matrix  WEX(k,kf) and backward De-excitation WDX(kf,k).  Notations: (j,k=Lower) --> (j, kf>k)  
      do j = FSS(nX), HSS(nX)
        do k = nuGS(j,nX), nuGS(j+1,nX) -1    ! Lower level is non-AI EL of "j"; 
          if(BE(k) .LT. 1.d-3)  cycle         ! skip dead lower; leave WEX(k,kf)=0
          do kf= k+1,   NST(nX)               ! thru ELs above "k", part of them have another ion charge  
            if(mthdEX(k,kf,nX).eq.-7) cycle   ! "-7" means that transition not described in "ExcXE...inp":  leave WEX=0
            if(kf.LT.Nnu(nX) .AND.            ! true EL has BE 
     +         BE(kf).LT.1.d-3)    cycle      ! skip dead candidate for Upper; leave WEX(k,kf)=0
            DE= E(kf,nX) - E(k,nX)            ! as both belong to same "j", thus have common GrSt    
            if(DE.le.0.) STOP 'DE<=0 in Ex1'  ! cannot happen because we check and correct "ELsXE...inp" in "Intro" subr. Later distorsion never observed.
            if(DE.GE.up) cycle                ! gap too large for Te(ti)
	      Ifail= -1
            rate= D01ahf(DE, up, tol, npts, rer, FVSEx, Nlim, Ifail)
            WEX(k,kf)= rate * Dene 
            rate= D01ahf(1.d-7, up, tol, npts, rer, FVSDx, Nlim, Ifail)
            WDX(kf,k)= rate * Dene 
          enddo
        enddo
        if(nuAS(j,nX) .GT. 0) then
          do k = kAI1(j,nX), kAI2(j,nX)-1  ! Lower level is AI EL of "j"; the last AIEL cannot be excited
            do kf= k+1, kAI2(j,nX)         ! Upper level must be AI(j) also; Note: (i) AIELs are treated as , 
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
c                                                      Algorithm, used for "Ejm" and "WDC", is checked by exact (in LTE) Griem3 (6.34) formula  
c                                                            WDC(k1,kAI) = WAiz(kAI,k1)*popLTE(kAI)/popLTE(k1).  
c                                       tCR comparison showed "==" in all POPs (but at that time we used "PI", not "PIR" for Ejm).
      do j = FSS(nX)+1, HSS(nX)       ! capturing ion       
        if(nuAS(j-1,nX).lt.1) goto 9       ! no AI ELs in SS=j-1          
        do k1 = nuGS(j,nX), nuGS(j+1,nX)-1    ! candidates for capturing EL, they are non-AI ELs
          if(BE(k1) .LT. 1.d-3)  cycle        ! dead EL
          do kAI= kAI1(j-1,nX), kAI2(j-1,nX)  ! AI ELs of SS=j-1.  Note: (i) AIELs are treated as "never-dead", 
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
	real(8) eeV, xw, yw, OM  ! OM is MFGu Collision Strength (Omega)
      SigInz= zero
      if(eeV.lt.BEk) STOP 'Came in SigInz with E < BEk'

      xw= eeV/BEk
      yw= one - one/xw  ! MF Gu 
      OM = Aix(k,kf,nX)*Dlog(xw)+ Bix(k,kf,nX)*yw*yw +   ! FAC guide (2.9)
     +     Cix(k,kf,nX)*yw/xw   + Dix(k,kf,nX)*yw/xw/xw
      SigInz= 3.8101e-16* OM /eeV /g0(k,nX)              ! FAC guide (2.10): "in A.U. e-imp SigInz= OM/k0^2/g(k)". 
c                                                          A.U. [Sig]= a0^2=2.8003(-17) cm2. 
      if(SigInz .lt. zero)   SigInz= zero      ! Bad fits to FAC points may have Sig < 0 even far from threshold 
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
      SigPhi= (Eix(k,kf,nX)+ Fix(k,kf,nX)/xw + Gix(k,kf,nX)/xw/xw) / 	    ! MFGu fit to SigPhi(x), see "GU expres for Cross-sec.pdf"
     +                                         xw**(3.5+Hix(k,kf,nX))       
      if(SigPhi.lt.zero) SigPhi= zero  ! Bad fits to FAC points may give Sig < 0 even far from threshold 
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
      real(8) eeV, As, Bs, Cs, Ds, Es, fReg, X, X2, Sig, Gaunt  
      As= Ax(k,kf,nX)
      Bs= Bx(k,kf,nX)
      Cs= Cx(k,kf,nX)
      Ds= Dx(k,kf,nX)
      Es= Ex(k,kf,nX)
c     Fs= Fx(k,kf,nX)      ! 5th coef not used im methods 0, 5, 11 
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

      SELECT CASE (Me)        ! # for fitting formula selection
      Case(0)                 ! Van Regemoter;  "ExcXE...inp" contains "f Regemor" > 0 for this (k,kf) couple.  
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

      Case(11)                                           ! in CODA SGM=(C+B*X1+A*X12)/(X1+D)**4/X1**E 
	  Sig= (Cs +Bs*X +As*X2)/(X+Ds)**4 /X**Es 
      END SELECT

      if(Sig .lt. zero) Sig= zero  ! bad fits to FAC points can result in Sig < 0 even far from threshold 
      SigExc= Sig  
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


 
      real(8) FUNCTION Voigt2(fwhmL, fwhmG, vC, dv) ! [eV], dv= v-vC, Sasha's connector to JELRT Drayson's VOIGT(X,Y); WELL checked
	use mo1                                       
	implicit none
	real(8) fwhmL, fwhmG, vC, dv, balf, x1, y1, Voi2, VOIGT
      balf= (vC/fwhmG)*1.6651092   ! 1.6651092= 2*Sq[ln(2)]
      x1= balf*abs(dv)/vC	         ! to Dr' units
      y1= 1.665109*fwhmL/2./fwhmG  ! to Dr' units
      Voi2= VOIGT(x1,y1)/1.772454  ! with renorm from "sqrt(pi)=1.7724539; Drayson" to 1 
      Voigt2= Voi2*balf/vC	     ! to "eV"
      END


      real(8) FUNCTION VOIGT(X,Y) ! Voigt shape normalized to sqrt(pi); From Drayson, JELRT, v.16, pp.611-614, 1976
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







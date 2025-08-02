      use mo1
      implicit none

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




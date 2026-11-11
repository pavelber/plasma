c  Co2feb26 (Code2, Generic version): main program.
c  All subroutines and functions are in Co2feb26_procs.for (split for testing, Nov 2026).
      use mo1co2feb
      implicit none
      CALL AllocArrays() ! Generic version: array sizes from Params0.inp (max NST, max HSS) at run time
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

      if(ti.GE.StopTime) then
         CALL FixLeadingZeros()   ! .ddd instead of 0.ddd in output files, as in the old MS Fortran build (lzfix_Co2_GNU.f90)
         STOP '   ti >= StopTime.  The End.'
      endif

      do La = 1, LaMx     ! La is zone number. La=1 is BS, La=2 is Core, La=3 is Capsule     
        CALL SCENARIO()   ! for t= ti compute R, Te, TD, DenI, ZC1, Dene
        write(*,'(a33, i2, e11.4, 2f7.0, e11.4, f10.4)') 
     +          'Zone, R(cm), Te, TD, ne, Z(X) =', 
     +           La, CeR(La), Te(La), TD(La), Dene(La), ZC(1,La)  

        write(30+La,'(f7.2, e10.3, 2f7.3, e11.4, 2f10.5, 3f7.2, e11.4,   ! files "BSinfo.dat", "CoreInfo.dat", "CapInfo.dat"      
     +                 3e10.3, f6.3, 2f8.2)')  ti*1.d12,  CeR(La)*1.d4, 
     +                      Te(La)/1.d3, TD(La)/1.d3, Dene(La), 
     +                 ZC1(La), ZC(1,La), ZC(2,La), ZC(3,La), ZC(4,La),  ! ZC(nX,La) is mean ion charge of (nX,La), computed in Scenario  
     +                      Den(1,La), Den(2,La), Den(3,La), Den(4,La),  ! using "POPi", see "POPi(k,nX,La) = POPf(k,nX,La)" just there
     +                                bp(La), bc(La)/1.d3, bw(La)/1.d3   ! [keV]

        do nX = 1, nXkin ! nXE  ! For Te=Ti, Den(nX,La) of t = ti. Note: only X has lines and DPI   *********** !!!! *********
           CALL LevStW()    ! For these La,nX compu FWHM [eV] of all E-LEVELs "LvJW(La,nX,k)" [eV] 

           CALL redPI()     ! For these La,nX, for each jSS compu DPI(jSS,nX,La) and PIR(jSS,nX,La).  

           CALL BEvPr()     ! For these La,nX compu Binding Energy of outermost electron "BE(k,nX,La)".
c                             Move POPs from BE-cut LEs to ground state of next SpS.

c          Gaussian part of FWHM/hvC of lines of #nX in zone #La is denoted "FWkVcGAU(La,nX)"
c             Amass = AtMass(nX) *Uam                     ! [g] mass of nX ion
c             uTi   = sqrt(8.*BolEr*Tion(La)/pin/Amass)   ! [cm/s] mean thermal velocity of nX ion in La
c             fwTiKvC(La,nX)= 1.4757*uTi/c                ! my page on thermal and isotropic 3D hydro broadenings                        
c             fwTUKvC(La,nX)= 1.4757*u3D(La)/c
c             FWkVcGAU(La,nX)= sqrt(fwTiKvC(La,nX)**2 +fwTUKvC(La,nX)**2)  ! FWHMgau/hvC of 2 convolved Gaussians.
           FWkVcGAU(La,nX)= 7.7155e-5*sqrt(TD(La)/AtMass(nX))  ! my page on thermal & hydro broadenings, see BlackFold   
        enddo  ! nX

        CALL EmiAbso()  ! Compu this-La "EmTot(La,hv)" [W/cc/sr/eV] and "AbTot(La,hv)" [1/cm]
c                         via params of t=ti and POPi(k,nX,La).  
      enddo  ! La loop;   La is zone number; La=1 is BS, La=2 is Core, La=3 is Capsule       

      CALL EffSpIn()  ! Compute due-to-all-La "SpInEf(La,iv)" [W/eV/sr/cm2] in RP of each "La", 
c                       La-loop is inside this subr because each "La" has contribs from other "La"s.  
c                      "SpInEf(La,iv)" is needed for Win, Wab, WphI, WiRR at t= ti" for d02 of POPs towards "tf".

      CALL PowYie()   ! Compute Spectral Power from the target using EmTot, AmTot computed
c	                  for params of t=ti and POPi(k,nX,La).  Print frames in files (181 - (186 and power detectors in files (187 - 189 	 

      do La = 1, LaMx	 
      do nX = 1, nXkin  ! nXE  ! ******** !!!! ****   
cccc         write(*,'(a30, 2i3, e10.2)') 'La, nX, Dene(La) =',           !@#$%*******************
cccc     +                                 La, nX, Dene(La)        
         if(Dene(La).gt.1.e15) then  ! For each (La,nX) compute Ws & PM; run d02 for POPs from "ti" to "tf" with params of "ti".  
c                                    ! Note: ne > 10^15 e/cc excludes inrerruption in NAG d02EAF. For some {Te, dTe/dt, Jv} this NAG subr may work even for lower ne       
cccc                 write(*,'(a30)') 'CALLing Atkins'         !@#$%***********************
               CALL AtKins()  ! It gives "POPf(k,nX,La)" that is POP(tf) to be first used in Scenario of NEXT "ti" as POPi(k,nX,La).
cccc                 write(*,'(a30)') 'Returned from Atkins'   !@#$%***********************
         endif
      enddo   
      enddo   
cccc              write(*,'(a30)') 'Returning to mark 1'   !@#$%***********************
      goto 1  ! for next t-step, where present "tf" will be "ti", thus present "POPf" will be used for computation of all quantities at "ti" 
      END     ! MAIN

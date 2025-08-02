
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

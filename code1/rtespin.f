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
         if(abs(abTot(2,iv)) .gt. 1.d-20)  ! exclude "/0." in next line;  La=2 is BS,
     +      So2 = emTot(2,iv)/abTot(2,iv)  ! in case of POPs inversion: "abTot" can be < 0  (alfa > 1) but
c                                            "SpOut" remains > 0 because [1-dexp...] is also negative
         RadFlux(iv) = 0.
         do k = 1, 10
            exw = dexp( -abTot(2,iv)* pathBS(k) )
            RadFlux(iv) = RadFlux(iv) + ArK(k)*So2*(1.-exw)   ! [W/eV/sr] from BS tovards a detector
         enddo
         SpOut1(iv) = RadFlux(iv)/ArBS  ! More convenient are [W/eV/sr/cm2] units, therefore relate "RadFlux" to towards-detector area of BS,
      enddo   ! iv
      Return
      END     ! of subr 'RTeSpIn"

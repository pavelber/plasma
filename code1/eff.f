      SUBROUTINE EffSpInK13()  ! 4pi-average RF "SpInEff(hv)" [W/eV/sr/cm2] for  Win, Wab, WphI and WiRR
      use mo1                  ! Here we use 119-point fit to spherical K13 function K=K(kD/2)
      implicit none
      real(8) K13, So, tau     ! SpInB

      if (La.ne.2) STOP 'Asked SpInEff for La =/= 2'

      do iv= 1, nvM
         SpInEff(2,iv) = zero    ! La=2 is BS
         tau= abTot(2,iv)* R2              ! can be < 0 in case of POPs inversion, see 3 lines below
         if(abs(abTot(2,iv)) .gt. 1.d-20)  ! exclude /0.
     +      So= emTot(2,iv)/abTot(2,iv)    ! [W/cc/sr/eV] / [1/cm].  If"abTot" < 0 due to POPs inversion (alfa > 0), "So" is also < 0 but
c                                                       ! "SpInEff" remains > 0 because tau < 0 made (1-dexp(-tau) < 0
         SpInEff(2,iv)= So*(one- dexp(-tau))* K13(tau)  ! spherical K13; For tau < 0 it takes K13= Ksp(1)= 0.75

         if(ng .eq. gpEq(PrFr))                    ! "ng" is serial number of "tf" point (thus, the number of "ti-tf" interval)
     +       write(452,'(e13.7, 7e11.4)') hvV(iv),                      ! "gpEq(j)" is NUMBER of t-point at which tf = tres(j)
     +                         SpInEff(2,iv)/1.d6, tau                  ! MW/cm2/sr/eV
      enddo
      Return
      END     ! 'EffSpIn' subr


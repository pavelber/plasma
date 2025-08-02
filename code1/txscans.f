      SUBROUTINE TXscans(FrNu) ! Simulate a scan of TX using RadFlux [W/eV/sr] from entier round area of BS. No Doppler shift
      use mo1
      implicit none
      integer i2, FrNu
      real(8) MCPor(5,nvM), MCP(nvM),  ScaF

c  Print "RadFlux" at "FrNu" time [in file (60+ that is "FuFluxFr#.dat"]
c  Note: RadFlux(iv) is computed in SUBR "RTeSpIn()" but I moved the print here because
c  SUBR "RTeSpIn()" works at each t, but TX scan is needed only at six tres(#) .

	write(60+FrNu,'(a38)') 'hvEV       iv   RadFluxW    KapD' ! "RadFlux(hv)" [W/eV/sr] is known in full interval of hv
      do iv= 1, nvM
         write(60+FrNu, '(f12.2, i7, 9e11.4)') hvV(iv), iv,              ! eV;
     +                             RadFlux(iv), abTot(2,iv)*2*R2         ! [W/eV/sr] from 1 BS; Opacity along central LOS of BS (La=2), 3rd index = 1 means motionless
      enddo
      close(60+FrNu)

c  hv-scans along thru-BS LOS done via "SpOut1(iv)" [W/sr/eV/cm2] calculated in SUBR "RTeSpIn" as
c  "SpOut1(iv)"= RadFlux(iv)/(pi*R2^2); RadFlux is the 10-r-zone  sum
c  For comparison to TREX spectra, add contributions fron higher (here: 2,3,4,5) orders of mica reflection

      ReTX   = zero       ! TREX respo (product) in each order of hvV(iv)
      do iv = iTX1, iTX2  ! on hv axis of the code BUT only within hv-interval in which the TREX response (product) is given
        hveV= hvV(iv)	    ! find values of "ReTX(order, hvPoint) on hv axis points
        do i2 = 1, npTX-1                                        ! hv-points in input file
           if(hvTX(i2).le. hveV .and. hveV.lt.hvTX(i2+1)) then   ! captured i2 and i2+1 TX point## around "hveV"
              frac= (hveV -hvTX(i2))/(hvTX(i2+1)-hvTX(i2))       ! for linear interpolation
              do j = 1, 5                                        ! for each order of mica refl
                ReTX(j,iv)= RespTX(j,i2)+                        ! TREX product (in each order of refl) at hv points
     +                     (RespTX(j,i2+1)-RespTX(j,i2))*frac
              enddo
           endif
        enddo
      enddo   ! iv

      SpOutOr= zero       ! "SpOut1" taken at j*hveV; j=1,2,3,4,5 , then plotted vs hv
      do iv = iTX1, iTX2  ! on hv axis of the code BUT only within hv-interval in which the TREX response (product) is given
        hveV= hvV(iv)
        do j = 1, 5             ! for each order of mica refl
          do i2 = iTX1-2, nvM-1
            if(hvV(i2).le. j*hveV .and. j*hveV.lt.hvV(i2+1)) then  ! captured hv point## (i2 and i2+1) around j*hveV
              frac= (j*hveV -hvV(i2))/(hvV(i2+1)-hvV(i2))          ! for linear interpolation
              SpOutOr(j,iv)=  SpOut1(i2  ) +                       ! SpOut1 at j*hveV; j=1,2,3,4,5; "SpOut1(iv)" is 10-r-zones area-mean SpIn from BS [W/sr/eV/cm2].
     +                       (SpOut1(i2+1) - SpOut1(i2)) *frac
                MCPor(j,iv)= SpOutOr(j,iv) * ReTX(j,iv)/ReTX(1,iv) ! on-TX-MCP contrib of j's order.
c                                                                  Note 1: here the units are [W/eV/sr/cm2] like "SpOut1"
c                                                                  Note 2: "SpOut1(iv)" == RadFlux/(piR2^2) is 10-r-zones area-mean SpIn from BS [W/sr/eV/cm2].
c                                                                  Note 3: here "/ReTX(1,iv)" is Itsic's and Sandia algorithm, see "YM explanation to compu TREX scans".
c                                                                          This "/ReTX(1,iv)" provides phys units [W/eV/sr/cm2] for "MCP", see 3 lines lower
            endif
          enddo   ! i2
          MCP(iv) = MCP(iv) + MCPor(j,iv)    ! on-TX-MCP at y=0: signal in [W/eV/sr/cm2] like 1st order + extra from 2,3,4,5
        enddo  ! j
      enddo    ! iv

      do iv= iTX1, iTX2
         if(hvV(iv) .lt. 1770.)  iv1770 = iv+1  ! conti after MgLyB
      enddo

      ScaF= 2.42  /MCP(iv1770)  ! 1520, Scaling Factor for calibration of MCP(iv) [W/eV/sr/cm2] to
c                                       2.42 [arb.u.] in expt TX scan at hv=1770eV that is continuum on blue side of MgLyB
c     ScaF= 1.3d-6/MCP(iv1770)  ! 1907, Same for shot 1907 where expt = 1.3d-6 [arb.u.] at 1770eV in Fr A6,B1

      write(170+FrNu,'(a29)') 'hvKeV     TXscan    KappaD' ! Full-D scan of TREX (based on RadFlux)
      do iv= 1, nvM
         if(hvV(iv).GT.1250. .and. hvV(iv).LT. 2100.)                    ! this interval is a bit wider tnan TX resolves
     +             write(170+FrNu,'(f9.6, 2e11.4)') hvV(iv)/1.d3,        ! keV;  											 ,
     +                                              MCP(iv)*ScaF,        ! Signal [expt. arb.u.] on TX due to 5 orders of mica reflection, calibrated to 2.42 at 1770 eV
     +                                         abTot(2,iv)* 2*R2         ! Opacity along central LOS of uniform spheric plasma (La=2),
      enddo ! iv
      close(170+FrNu)

      if(FrNu.LT.mSpe) write(*,'(/a50)')
     +       'tf[ns]  D[mm]   Te[eV]  TD[keV]    i/cc     %Mg'

      if(FrNu .eq. mSpe) then  ! The end of the computations
             write(*,'(/a53/)') 'Finished the Computation.'
             STOP
      endif
      Return
      END     ! of subr "TXscans(FrNu)"



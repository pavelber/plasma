!  Code2 (Co2feb26), Generic version. Module split (Nov 2026):
!    co2_const  - constants and fixed dimensions
!    co2_atom   - atomic database of the elements, rate matrices and populations,
!                 channel lists; their allocation (AllocArrays, AllocSized, AllocLines)
!    co2_util   - wall-clock timers, integration context of D01AHF, FlushAll
!    mo1co2feb  - state of the plasma, grids, spectra, run control; it uses the three
!                 modules above, so 'use mo1co2feb' gives access to everything.
      module co2_util
      implicit none
!  ---- Integration context (Nov 2026): the channel whose rate D01AHF integrates.
!       FVSinz, FVStbr, FVSEx, FVSDx read it (they can have only one argument).
!       Each OpenMP thread has its own copy (threadprivate).
      integer, public :: cK= 0, cKf= 0, cJ= 0
      real(8), public :: cBE= 0.d0, cDE= 0.d0
!$omp threadprivate(cK, cKf, cJ, cBE, cDE)

!  ---- Wall-clock timers (Nov 2026). TimOn(i)/TimOff(i) around the main parts of
!       a time step; TimReport writes one line per time step to Timing.log.
!       They only measure time and do not change any result.
      integer, public, parameter :: nTim = 20
      character(9), public, parameter :: tName(nTim) = (/ &
     & 'Intro    ','LineList ','hvPoints ','SCENARIO ','LevStW   ', &
     & 'redPI    ','BEvPr    ','EmiAbso  ','EA_BB    ','EA_BF    ', &
     & 'EffSpIn  ','PowYie   ','AtKins   ','Ws       ','Ws_lines ', &
     & 'Ws_ioniz ','Ws_excit ','Ws_DC    ','PMg0     ','D02EAF   ' /)
      real(8), public :: tAcc(nTim)= 0.d0, tBeg(nTim)= 0.d0, &
     &                   tLast(nTim)= 0.d0, tRep= -1.d0, tRun0= -1.d0, &
     &                   tiStep= 0.d0

      contains

      real(8) FUNCTION WallSec()  ! wall-clock time [s]
      integer(8) cnt, rate
      call system_clock(cnt, rate)
      WallSec= dble(cnt)/dble(rate)
      END FUNCTION WallSec

      SUBROUTINE TimOn(it)
      integer it
      if(tRun0 .lt. 0.d0) tRun0= WallSec()
      tBeg(it)= WallSec()
      END SUBROUTINE TimOn

      SUBROUTINE TimOff(it)
      integer it
      tAcc(it)= tAcc(it)+ WallSec()- tBeg(it)
      END SUBROUTINE TimOff

      SUBROUTINE FlushAll()  ! flush all open output units (Nov 2026): a run that is
      integer iu  ! stopped keeps the output of the finished time steps
      logical op
      do iu = 7, 999
        inquire(unit=iu, opened=op)
        if(op) flush(iu)
      enddo
      END SUBROUTINE FlushAll

      end module co2_util

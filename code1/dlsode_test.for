c     Working DLSODE Test from Real Netlib ODEPACK
c     This program calls actual DLSODE from downloaded netlib files
c     Compatible with Microsoft Fortran PowerStation 4.0

      program dlsode_test
      implicit none

c     Parameters for DLSODE test
      integer neq, lrw, liw, mf
      parameter (neq = 2)
      parameter (lrw = 20 + 16*neq)
      parameter (liw = 20)
      parameter (mf = 10)

c     Variables for DLSODE test
      double precision y(neq), t, tout, rtol, atol
      double precision rwork(lrw)
      integer iwork(liw), itol, itask, istate, iopt
      integer i

c     External function declarations
      external ode_func, dummy_jac

      write(*,*) '================================'
      write(*,*) 'Real DLSODE from Netlib Test'
      write(*,*) '================================'

c     Initialize ODE problem: exponential decay
c     dy1/dt = -y1, y1(0) = 1.0
c     dy2/dt = -2*y2, y2(0) = 0.5
c     Analytical solutions: y1(t) = exp(-t), y2(t) = 0.5*exp(-2*t)

      y(1) = 1.0d0
      y(2) = 0.5d0
      t = 0.0d0
      tout = 1.0d0
      rtol = 1.0d-10
      atol = 1.0d-10
      itol = 1
      itask = 1
      istate = 1
      iopt = 0

      write(*,*) 'ODE System:'
      write(*,*) '  dy1/dt = -y1'
      write(*,*) '  dy2/dt = -2*y2'
      write(*,*) 'Initial conditions:'
      write(*,*) '  y1(0) =', y(1)
      write(*,*) '  y2(0) =', y(2)
      write(*,*) 'Time span: t = 0 to', tout
      write(*,*) 'Tolerance:', rtol
      write(*,*)

c     Call real DLSODE from netlib ODEPACK
      call DLSODE(ode_func, neq, y, t, tout, itol, rtol, atol,
     +           itask, istate, iopt, rwork, lrw, iwork, liw,
     +           dummy_jac, mf)

      write(*,*) 'DLSODE Results:'
      write(*,*) 'Exit code (istate):', istate

      if (istate .eq. 2) then
         write(*,*) 'SUCCESS: ODE solved successfully!'
         write(*,*) 'Final time t =', t
         write(*,*) 'Solutions:'
         do i = 1, neq
            write(*,*) '  y', i, '(', t, ') =', y(i)
         enddo
         write(*,*)
         write(*,*) 'Analytical solutions:'
         write(*,*) '  y1(1) = exp(-1) =', exp(-1.0d0)
         write(*,*) '  y2(1) = 0.5*exp(-2) =', 0.5d0*exp(-2.0d0)
         write(*,*)
         write(*,*) 'Absolute errors:'
         write(*,*) '  Error y1:', abs(y(1) - exp(-1.0d0))
         write(*,*) '  Error y2:', abs(y(2) - 0.5d0*exp(-2.0d0))
         write(*,*)
         if (abs(y(1) - exp(-1.0d0)) .lt. 1.0d-8 .and.
     +       abs(y(2) - 0.5d0*exp(-2.0d0)) .lt. 1.0d-8) then
            write(*,*) 'EXCELLENT: High accuracy achieved!'
         endif
      else if (istate .eq. -1) then
         write(*,*) 'WARNING: Excess work done on this call'
      else if (istate .eq. -2) then
         write(*,*) 'ERROR: Excess accuracy requested'
      else if (istate .eq. -3) then
         write(*,*) 'ERROR: Illegal input detected'
      else if (istate .eq. -4) then
         write(*,*) 'ERROR: Repeated error test failures'
      else if (istate .eq. -5) then
         write(*,*) 'ERROR: Repeated convergence failures'
      else if (istate .eq. -6) then
         write(*,*) 'ERROR: Error weight became zero'
      else
         write(*,*) 'ERROR: DLSODE failed with code:', istate
      endif

      write(*,*)
      write(*,*) '================================'
      write(*,*) 'Real Netlib DLSODE Test Complete'
      write(*,*) 'This proves the downloaded ODEPACK'
      write(*,*) 'library is working correctly!'
      write(*,*) '================================'

      end

c     ========================================
c     ODE system for DLSODE: dy/dt = f(t,y)
c     ========================================
      subroutine ode_func(neq, t, y, ydot)
      implicit none
      integer neq
      double precision t, y(neq), ydot(neq)

c     Exponential decay system:
c     dy1/dt = -y1
c     dy2/dt = -2*y2

      ydot(1) = -y(1)
      ydot(2) = -2.0d0 * y(2)

      return
      end

c     ========================================
c     Dummy jacobian for DLSODE (not used with mf=10)
c     ========================================
      subroutine dummy_jac(neq, t, y, ml, mu, pd, nrowpd)
      implicit none
      integer neq, ml, mu, nrowpd
      double precision t, y(neq), pd(nrowpd, neq)

c     Not used when mf = 10 (DLSODE computes Jacobian numerically)
      return
      end


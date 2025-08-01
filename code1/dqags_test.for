program dqags_test
      implicit none

c     Test program for DQAGS (Netlib/QUADPACK)
c     Compatible with Microsoft Fortran PowerStation 4.0

c     External function to be integrated
      external func_for_int

c     DQAGS variables
      double precision a, b, epsabs, epsrel, result, abserr
      integer limit, neval, ier, last,i
      parameter (limit = 100)
      integer lenw
      parameter (lenw = 4 * limit)
      integer iwork(limit)
      double precision work(lenw)

c     Common variables
      double precision analytical_sol

      write(*,*) '=========================================='
      write(*,*) 'DQAGS (Netlib/QUADPACK) Test'
      write(*,*) '=========================================='
      write(*,*)
      write(*,*) 'Integrating f(x) = x*x from 0 to 1'
      write(*,*) 'Analytical solution = 1/3'
      analytical_sol = 1.0d0 / 3.0d0
      write(*,*) 'Analytical solution:', analytical_sol
      write(*,*)
      do i = 1, limit
          iwork(i) = 0
      end do
      do i = 1, lenw
          work(i) = 0.0d0
      end do
c
c     ---------------------------------
c     Test 1: DQAGS from Netlib/QUADPACK
c     ---------------------------------
      write(*,*) '--- Testing DQAGS (Netlib/QUADPACK) ---'
      a = 0.0d0
      b = 1.0d0
      epsabs = 1.0d-8
      epsrel = 1.0d-8

      call DQAGS(func_for_int, a, b, epsabs, epsrel, result,
     +           abserr, neval, ier, limit, lenw, last, iwork, work)

      write(*,*) 'DQAGS Results:'
      write(*,*) '  Result:', result
      write(*,*) '  Absolute error estimate:', abserr
      write(*,*) '  Number of evaluations:', neval
      write(*,*) '  Exit code (ier):', ier
      write(*,*) '  Actual absolute error:', dabs(result -
     +           analytical_sol)
      write(*,*)
c
      write(*,*) '=========================================='
      write(*,*) 'Test Complete'
      write(*,*) '=========================================='

      end

c     ========================================
c     Function to be integrated: f(x) = x*x
c     ========================================
      double precision function func_for_int(x)
      implicit none
      double precision x
      func_for_int = x * x
      return
      end

        DOUBLE PRECISION FUNCTION D1MACH(I)
        IMPLICIT NONE
        INTEGER I
        IF (I .EQ. 1) THEN
            D1MACH = 2.2250738585072014D-308 ! Smallest positive number
        ELSE IF (I .EQ. 2) THEN
            D1MACH = 1.7976931348623157D+308 ! Largest positive number
        ELSE IF (I .EQ. 3) THEN
            D1MACH = 1.1102230246251565D-16  ! Machine epsilon
        ELSE IF (I .EQ. 4) THEN
            D1MACH = 2.2204460492503131D-16  ! Smallest relative spacing
        ELSE IF (I .EQ. 5) THEN
            D1MACH = 0.3010299956639812D0    ! Log10(2)
        ELSE
            STOP 'D1MACH: Invalid argument'
        END IF
        RETURN
        END
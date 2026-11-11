! T1-D02 (Nov 2026): the D02EAF replacement (matrix exponential exp(A) y for the frozen
! rate matrix) against the previous algorithm D02EAF_TAYLOR, on random rate matrices of
! several sizes and norms ||A||_1 from 1e-3 to 1e6 (s = 0 ... 21 squarings).
! For n <= 60 also an exact reference in quadruple precision (real(16)).
! For each case it prints: n, norm, rel.diff new-old, |sum(y_new)-1|, min(y_new),
! ifail new, ifail old, rel.error new, rel.error old (vs quad; -1 if no quad reference),
! |sum(y_old)-1|. run_tests.py checks the limits.
program test_d02
  use mo1co2feb
  implicit none
  external POPdot
  integer, parameter :: nmax = 400
  integer :: sizes(3) = (/ 6, 60, 400 /)
  real(8) :: norms(8) = (/ 1.d-3, 0.3d0, 1.d0, 30.d0, 1.d3, 3.d4, 1.d5, 1.d6 /)
  real(8), allocatable :: y0(:), ynew(:), yref(:)
  real(8) :: r, nrm, rel, dsum, dsumo, errn, erro
  real(16), allocatable :: yq(:)
  integer :: isz, inr, n, ia, ja, f1, f2, seed
  character(256) :: outf
  integer :: uo

  outf = 'd02_out.txt'
  if (command_argument_count() >= 1) call get_command_argument(1, outf)
  open(newunit=uo, file=trim(outf), status='replace')
  call AllocSized(nmax, 4)
  Scale = 1.d40
  seed = 12345
  do isz = 1, 3
    n = sizes(isz)
    allocate(y0(n), ynew(n), yref(n), yq(n))
    ! random rate matrix PM(source, destination): rates spread over 8 decades,
    ! sparse like an atomic model (about 30% of the pairs connected)
    PM = 0.d0
    do ia = 1, n
      do ja = 1, n
        if (ia /= ja) then
          r = urand(seed)
          if (r < 0.3d0) PM(ia,ja) = 10.d0**(8.d0 + 8.d0*urand(seed))
        end if
      end do
    end do
    do ia = 1, n
      PM(ia,ia) = -sum(PM(ia,1:n))
    end do
    nrm = maxval(sum(abs(PM(1:n,1:n)), dim=2))       ! ||A||_1 for dt = 1 s
    do ia = 1, n
      y0(ia) = urand(seed)
    end do
    y0 = y0 / sum(y0)
    do inr = 1, 8
      ynew = y0;  yref = y0
      f1 = -1;  f2 = -1
      call D02EAF(0.d0, norms(inr)/nrm*Scale, n, ynew, tolD02, POPdot, WEAF, Nwork, f1)
      call D02EAF_TAYLOR(0.d0, norms(inr)/nrm*Scale, n, yref, tolD02, POPdot, WEAF, Nwork, f2)
      rel  = maxval(abs(ynew - yref)) / maxval(abs(yref))
      dsum = abs(sum(ynew) - 1.d0)
      dsumo= abs(sum(yref) - 1.d0)
      errn = -1.d0;  erro = -1.d0
      if (n <= 60) then
        call expq(n, norms(inr)/nrm, y0, yq)
        errn = dble(maxval(abs(real(ynew,16) - yq)) / maxval(abs(yq)))
        erro = dble(maxval(abs(real(yref,16) - yq)) / maxval(abs(yq)))
      end if
      write(uo,'(i5,es10.2,3es12.3e3,2i3,3es12.3e3)') n, norms(inr), rel, dsum, minval(ynew), &
        f1, f2, errn, erro, dsumo
    end do
    deallocate(y0, ynew, yref, yq)
  end do
  close(uo)
contains
  subroutine expq(n, dt, y, yq)       ! exp(dt*PM^T) y in quadruple precision
    integer, intent(in) :: n          ! (scaling to norm <= 1e-3, 40 Taylor terms, squarings)
    real(8), intent(in) :: dt, y(n)
    real(16), intent(out) :: yq(n)
    real(16), allocatable :: a(:,:), e(:,:), t(:,:)
    real(16) :: nq
    integer :: s, k, i
    allocate(a(n,n), e(n,n), t(n,n))
    a = transpose(real(PM(1:n,1:n),16)) * real(dt,16)
    nq = maxval(sum(abs(a), dim=1))
    s = max(0, ceiling(log(max(nq, 1.q-300)/1.q-3)/log(2.q0)))
    a = a / 2.q0**s
    e = 0;  t = 0
    do i = 1, n
      e(i,i) = 1;  t(i,i) = 1
    end do
    do k = 1, 40
      t = matmul(t, a) / real(k,16)
      e = e + t
    end do
    do k = 1, s
      e = matmul(e, e)
    end do
    yq = matmul(e, real(y,16))
    deallocate(a, e, t)
  end subroutine

  real(8) function urand(sd)          ! simple reproducible generator (Park-Miller)
    integer, intent(inout) :: sd
    integer(8) :: t
    t = mod(16807_8 * int(sd, 8), 2147483647_8)
    sd = int(t)
    urand = dble(sd) / 2147483647.d0
  end function
end program test_d02

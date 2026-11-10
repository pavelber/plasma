! nag_replacements_win.f90
! Stand-alone replacements for the two NAG routines used by Code1_AlMg_GNU_v01.for:
!   D01AHF  - definite integral of f(x) over [a,b]
!   D02EAF  - integration of the population rate equations over [x, xend]
! Written from the description in Code1_AlMg_GNU_v01_technical_migration_report.docx
! (the original nag_replacements_AlMg_GNU_v01.f90 was not available).
! These are NOT NAG and are not guaranteed to reproduce NAG results bit-for-bit.

!=======================================================================
! D01AHF: globally adaptive 15-point Gauss-Kronrod quadrature.
!   a, b   - limits;  epsr - requested relative accuracy
!   npts   - (out) number of function evaluations
!   relerr - (out) estimated relative error
!   f      - real(8) function f(x)
!   nlimit - max. evaluations (<=0 means default); ifail - 0 ok, 1 limit reached
!=======================================================================
real(8) function D01AHF(a, b, epsr, npts, relerr, f, nlimit, ifail)
  implicit none
  real(8), intent(in)    :: a, b, epsr
  integer, intent(out)   :: npts
  real(8), intent(out)   :: relerr
  integer, intent(in)    :: nlimit
  integer, intent(inout) :: ifail
  real(8), external      :: f

  integer, parameter :: maxint = 2000
  real(8) :: lo(maxint), hi(maxint), val(maxint), err(maxint)
  real(8) :: total, toterr, eps, r, e, m
  integer :: nint, imax, maxev

  eps   = max(epsr, 1.d-12)
  maxev = 15*maxint
  if (nlimit > 0) maxev = max(nlimit, 15*20)
  npts  = 0
  ifail = 0
  relerr = 0.d0
  if (b == a) then
     D01AHF = 0.d0
     return
  end if

  nint = 1
  lo(1) = a;  hi(1) = b
  call gk15(f, a, b, val(1), err(1))
  npts = 15
  do
     total  = sum(val(1:nint))
     toterr = sum(err(1:nint))
     if (toterr <= eps*abs(total) .or. toterr <= 1.d-300) exit
     if (nint >= maxint .or. npts + 30 > maxev) then
        ifail = 1
        exit
     end if
     imax = maxloc(err(1:nint), 1)            ! bisect the worst interval
     m = 0.5d0*(lo(imax) + hi(imax))
     if (m <= lo(imax) .or. m >= hi(imax)) then  ! cannot split any further
        ifail = 1
        exit
     end if
     nint = nint + 1
     lo(nint) = m;          hi(nint) = hi(imax)
     hi(imax) = m
     call gk15(f, lo(imax), hi(imax), r, e);  val(imax) = r;  err(imax) = e
     call gk15(f, lo(nint), hi(nint), r, e);  val(nint) = r;  err(nint) = e
     npts = npts + 30
  end do

  D01AHF = total
  if (abs(total) > 0.d0) relerr = toterr/abs(total)
contains
  subroutine gk15(f, x1, x2, res, abserr)
    real(8), external :: f
    real(8), intent(in)  :: x1, x2
    real(8), intent(out) :: res, abserr
    real(8), parameter :: xgk(8) = [ 0.991455371120812639206854697526329d0, &
         0.949107912342758524526189684047851d0, 0.864864423359769072789712788640926d0, &
         0.741531185599394439863864773280788d0, 0.586087235467691130294144845693013d0, &
         0.405845151377397166906606412076961d0, 0.207784955007898467600689403773245d0, &
         0.000000000000000000000000000000000d0 ]
    real(8), parameter :: wgk(8) = [ 0.022935322010529224963732008058970d0, &
         0.063092092629978553290700663189204d0, 0.104790010322250183839876322541518d0, &
         0.140653259715525918745189590510238d0, 0.169004726639267902826583426598550d0, &
         0.190350578064785409913256402421014d0, 0.204432940075298892414161999234649d0, &
         0.209482141084727828012999174891714d0 ]
    real(8), parameter :: wg(4) = [ 0.129484966168869693270611432679082d0, &
         0.279705391489276667901467771423780d0, 0.381830050505118944950369775488975d0, &
         0.417959183673469387755102040816327d0 ]
    real(8) :: c, h, fc, f1, f2, rk, rg
    integer :: j
    c  = 0.5d0*(x1 + x2)
    h  = 0.5d0*(x2 - x1)
    fc = f(c)
    rk = wgk(8)*fc
    rg = wg(4)*fc
    do j = 1, 7
       f1 = f(c - h*xgk(j))
       f2 = f(c + h*xgk(j))
       rk = rk + wgk(j)*(f1 + f2)
       if (mod(j, 2) == 0) rg = rg + wg(j/2)*(f1 + f2)   ! Gauss nodes are xgk(2,4,6,8)
    end do
    res    = rk*h
    abserr = abs((rk - rg)*h)
  end subroutine gk15
end function D01AHF

!=======================================================================
! D02EAF: advance populations y over [x, xend] (scaled time) with the
! transition matrix PM of module mo1 held constant during the step:
!     dy(i)/dt = sum_j PM(j,i) y(j)   =>   y(xend) = exp(A*dt) y(x),  A = transpose(PM)
! dt = (xend - x)/Scale.  exp(A*dt) is computed by uniformization + scaling and
! squaring: with lam >= max(-A(i,i)), P = (A + lam*I)*tau is non-negative, so
! exp(A*tau) = exp(-lam*tau) * sum_k P^k/k!  has only non-negative terms (no
! cancellation, populations stay >= 0), then it is squared s times.
! fcn (POPdot), tol, w, iw are accepted for interface compatibility only.
!=======================================================================
subroutine D02EAF(x, xend, n, y, tol, fcn, w, iw, ifail)
  use mo1, only: PM, Scale
  implicit none
  integer, intent(in)    :: n, iw
  real(8), intent(inout) :: x, y(n)
  real(8), intent(in)    :: xend, tol
  real(8), intent(inout) :: w(*)
  integer, intent(inout) :: ifail
  external :: fcn

  real(8), allocatable :: Am(:,:), P(:,:), T(:,:), E(:,:)
  real(8) :: dt, lam, tau, tnorm, ynew(n)
  integer :: i, k, s
  integer, parameter :: kmax = 60

  ifail = 0
  dt = (xend - x)/Scale
  if (dt <= 0.d0 .or. n <= 0) then
     x = xend
     return
  end if

  allocate(Am(n,n), P(n,n), T(n,n), E(n,n))
  Am = transpose(PM(1:n,1:n))
  lam = 0.d0
  do i = 1, n
     lam = max(lam, -Am(i,i))
  end do

  if (lam*dt <= 0.d0) then
     x = xend
     return
  end if

  s = max(0, ceiling(log(lam*dt)/log(2.d0)))     ! lam*tau <= 1
  if (s > 200) then
     ifail = 1
     write(*,*) 'D02EAF replacement: too many squarings, s=', s
     return
  end if
  tau = dt/2.d0**s

  P = Am*tau
  do i = 1, n
     P(i,i) = P(i,i) + lam*tau
  end do

  ! Taylor series of exp(P); all terms >= 0 (up to tiny negative off-diagonals, if any)
  E = 0.d0
  do i = 1, n
     E(i,i) = 1.d0
  end do
  T = E
  do k = 1, kmax
     T = matmul(P, T)/dble(k)
     E = E + T
     tnorm = maxval(sum(abs(T), dim=1))
     if (tnorm < 1.d-18) exit
  end do
  E = E*exp(-lam*tau)

  do k = 1, s
     E = matmul(E, E)
  end do

  ynew = matmul(E, y)

  if (any(ynew /= ynew)) then
     ifail = 2
     write(*,*) 'D02EAF replacement: NaN in populations'
     return
  end if
  if (minval(ynew) < -1.d-10) then
     ifail = 2
     write(*,*) 'D02EAF replacement: negative population', minval(ynew)
  end if

  y = ynew
  x = xend
  deallocate(Am, P, T, E)
end subroutine D02EAF

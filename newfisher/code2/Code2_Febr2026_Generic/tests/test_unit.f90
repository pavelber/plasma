! T1: unit tests of the physics functions of Code2 (runs in about a second).
! The functions get their inputs partly as arguments and partly through module
! mo1co2feb (k, kf, j, BEk, DE, nX, La, Te, fit coefficients ...). Here that
! state is set to synthetic values that cover all branches of each function;
! every result is written bit for bit to the output file, which run_tests.py
! compares with golden/unit.txt.
program test_unit
  use mo1co2feb
  use testio
  implicit none
  real(8), external :: EED, SigPhi, SigInz, SigExc, FVSinz, FVStbr, FVSEx, &
                       FVSDx, Voigt2, VOIGT, Gauss, Loren, D01AHF
  external POPdot
  real(8) :: Tes(3), bps(2), ees(12), BEs(2), xf(5), cPhi(4,3), cInz(4,3)
  real(8) :: cExc(6,6), DEs(2), xe(6), vX(8), vY(9), fL(4), fG(3), dvs(8)
  integer :: mEx(6)
  real(8) :: r, rr, yv(6), dts(3)
  integer :: it, ibp, ie, iBE, mx, ic, id, il, ig, jv, np, nl, ifl, ia, ja
  character(64) :: nm
  character(256) :: outf

  outf = 'unit_out.txt'
  if (command_argument_count() >= 1) call get_command_argument(1, outf)
  call topen(trim(outf))

  ! small arrays: 6 levels, H-like SpS 4
  call AllocSized(6, 4)
  nX = 1;  La = 1;  NST = 6;  Nnu = 6
  KiSS(:,1) = (/ 1, 1, 1, 2, 2, 3 /)
  g0(:,1)   = (/ 2.d0, 6.d0, 10.d0, 1.d0, 3.d0, 1.d0 /)
  E(:,1)    = (/ 0.d0, 12.d0, 30.d0, 0.d0, 9.d0, 0.d0 /)
  k = 1;  kf = 4;  j = 1
  bra(1,4,1) = 1.d0
  SigMax(1) = 1.d-15
  Sah(1) = 3.7d-4

  Tes = (/ 3.d0, 300.d0, 5000.d0 /)
  bps = (/ 0.d0, 0.2d0 /)
  ees = (/ 1.d-6, 0.1d0, 1.d0, 10.d0, 100.d0, 1.d3, 1.75d3, 2.d3, 2.25d3, 3.d3, 1.d4, 1.d5 /)

  ! --- EED: Maxwell and Maxwell + e-beam
  do it = 1, 3
    do ibp = 1, 2
      Te(1) = Tes(it);  bp(1) = bps(ibp);  bc(1) = 2.d3;  bw(1) = 5.d2
      do ie = 1, 12
        write(nm,'(a,3i3)') 'EED', it, ibp, ie
        call rec(nm, EED(ees(ie)))
      end do
    end do
  end do

  ! --- SigPhi: threshold region, tail, clamps to 0 and to SigMax
  BEs = (/ 5.d0, 800.d0 /)
  xf  = (/ 1.0001d0, 1.5d0, 3.d0, 10.d0, 100.d0 /)
  cPhi(:,1) = (/ 1.d-18, 2.d-19, -1.d-19, 0.2d0 /)
  cPhi(:,2) = (/ -1.d-17, 0.d0, 0.d0, 0.d0 /)
  cPhi(:,3) = (/ 1.d-10, 0.d0, 0.d0, -3.4d0 /)
  do ic = 1, 3
    Eix(1,4) = cPhi(1,ic); Fix(1,4) = cPhi(2,ic)
    Gix(1,4) = cPhi(3,ic); Hix(1,4) = cPhi(4,ic)
    do iBE = 1, 2
      BEk = BEs(iBE)
      do mx = 1, 5
        write(nm,'(a,3i3)') 'SigPhi', ic, iBE, mx
        call rec(nm, SigPhi(BEk*xf(mx)))
      end do
    end do
  end do

  ! --- SigInz, FVSinz, FVStbr
  cInz(:,1) = (/ 1.d0, 0.5d0, -0.2d0, 0.1d0 /)
  cInz(:,2) = (/ -5.d0, 0.d0, 0.d0, 0.d0 /)
  cInz(:,3) = (/ 1.d5, 3.d4, 0.d0, 0.d0 /)
  xf = (/ 1.d0, 1.01d0, 2.d0, 10.d0, 1.d3 /)
  Te(1) = 300.d0;  bp(1) = 0.2d0;  bc(1) = 2.d3;  bw(1) = 5.d2
  do ic = 1, 3
    Aix(1,4) = cInz(1,ic); Bix(1,4) = cInz(2,ic)
    Cix(1,4) = cInz(3,ic); Dix(1,4) = cInz(4,ic)
    do iBE = 1, 2
      BEk = BEs(iBE)
      do mx = 1, 5
        write(nm,'(a,3i3)') 'SigInz', ic, iBE, mx
        call rec(nm, SigInz(BEk*xf(mx)))
        write(nm,'(a,3i3)') 'FVSinz', ic, iBE, mx
        call sync()   ! integration context = module k, kf, j, BEk, DE
        call rec(nm, FVSinz(BEk*xf(mx)))
        write(nm,'(a,3i3)') 'FVStbr', ic, iBE, mx
        call sync()   ! integration context = module k, kf, j, BEk, DE
        call rec(nm, FVStbr(BEk*xf(mx)*0.01d0))
      end do
    end do
  end do

  ! --- SigExc for fit methods 5, 11 (also the X+D = 0 branch), 16; FVSEx, FVSDx
  k = 1;  kf = 2;  j = 1
  mEx = (/ 5, 5, 11, 11, 16, 16 /)
  cExc(:,1) = (/ 1.d-17, 2.d-18, -1.d-18, 3.d-19, 1.d-18, 0.d0 /)
  cExc(:,2) = (/ -1.d-16, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 /)
  cExc(:,3) = (/ 1.d-17, 3.d-17, 2.d-16, 0.5d0, 0.3d0, 0.d0 /)
  cExc(:,4) = (/ 1.d-17, 3.d-17, 2.d-16, -1.d0, 0.3d0, 0.d0 /)
  cExc(:,5) = (/ 1.d-17, 5.d-18, 2.d-18, 1.d-18, 0.05d0, 2.d0 /)
  cExc(:,6) = (/ 4.d-16, 1.d-16, 5.d-17, 1.d-17, 0.2d0, 1.5d0 /)
  DEs = (/ 10.d0, 1000.d0 /)
  xe  = (/ 1.d0, 1.0001d0, 1.5d0, 3.d0, 30.d0, 300.d0 /)
  do ic = 1, 6
    MthdEX(1,2) = mEx(ic)
    Ax(1,2) = cExc(1,ic); Bx(1,2) = cExc(2,ic); Cx(1,2) = cExc(3,ic)
    Dx(1,2) = cExc(4,ic); Ex(1,2) = cExc(5,ic); Fx(1,2) = cExc(6,ic)
    do id = 1, 2
      DE = DEs(id)
      do mx = 1, 6
        write(nm,'(a,3i3)') 'SigExc', ic, id, mx
        call rec(nm, SigExc(DE*xe(mx)))
        write(nm,'(a,3i3)') 'FVSEx', ic, id, mx
        call sync()   ! integration context = module k, kf, j, BEk, DE
        call rec(nm, FVSEx(DE*xe(mx)))
        write(nm,'(a,3i3)') 'FVSDx', ic, id, mx
        call sync()   ! integration context = module k, kf, j, BEk, DE
        call rec(nm, FVSDx(DE*xe(mx)*0.1d0))
      end do
    end do
  end do

  ! --- D01AHF with the same integrands and limits as in Ws
  do it = 2, 3
    Te(1) = Tes(it);  bp(1) = 0.d0
    upTB = 200.*Te(1)
    k = 1;  kf = 4;  j = 1;  BEk = 50.d0
    Aix(1,4) = 1.d0; Bix(1,4) = 0.5d0; Cix(1,4) = -0.2d0; Dix(1,4) = 0.1d0
    ifl = -1
    call sync()   ! integration context = module k, kf, j, BEk, DE
    rr = D01AHF(BEk, upTB, tol, np, r, FVSinz, nl, ifl)
    write(nm,'(a,i3)') 'D01_inz', it;  call rec(nm, rr);  call reci(trim(nm)//'_n', np)
    call sync()   ! integration context = module k, kf, j, BEk, DE
    rr = D01AHF(1.d-8, upTB, tol, np, r, FVStbr, nl, ifl)
    write(nm,'(a,i3)') 'D01_tbr', it;  call rec(nm, rr);  call reci(trim(nm)//'_n', np)
    k = 1;  kf = 2;  DE = 12.d0
    MthdEX(1,2) = 5
    Ax(1,2) = 1.d-17; Bx(1,2) = 2.d-18; Cx(1,2) = -1.d-18
    Dx(1,2) = 3.d-19; Ex(1,2) = 1.d-18; Fx(1,2) = 0.d0
    call sync()   ! integration context = module k, kf, j, BEk, DE
    rr = D01AHF(DE, upTB, tol, np, r, FVSEx, nl, ifl)
    write(nm,'(a,i3)') 'D01_ex', it;  call rec(nm, rr);  call reci(trim(nm)//'_n', np)
    call sync()   ! integration context = module k, kf, j, BEk, DE
    rr = D01AHF(1.d-8, upTB, tol, np, r, FVSDx, nl, ifl)
    write(nm,'(a,i3)') 'D01_dx', it;  call rec(nm, rr);  call reci(trim(nm)//'_n', np)
  end do

  ! --- VOIGT on a grid that covers all its branches; Voigt2, Gauss, Loren
  vX = (/ 0.d0, 0.3d0, 1.d0, 2.d0, 4.9d0, 5.d0, 7.d0, 12.d0 /)
  vY = (/ 1.d-3, 0.3d0, 0.99d0, 1.d0, 1.2d0, 1.5d0, 3.d0, 6.d0, 12.d0 /)
  do mx = 1, 8
    do jv = 1, 9
      write(nm,'(a,2i3)') 'VOIGT', mx, jv
      call rec(nm, VOIGT(vX(mx), vY(jv)))
    end do
  end do
  fL  = (/ 1.d-3, 0.1d0, 1.d0, 10.d0 /)
  fG  = (/ 0.05d0, 1.d0, 5.d0 /)
  dvs = (/ 0.d0, 0.01d0, 0.1d0, 1.d0, 3.d0, 10.d0, 100.d0, 300.d0 /)
  do il = 1, 4
    do ig = 1, 3
      do jv = 1, 8
        write(nm,'(a,3i3)') 'Voigt2', il, ig, jv
        call rec(nm, Voigt2(fL(il), fG(ig), 1000.d0, dvs(jv)))
      end do
    end do
  end do
  do jv = 1, 8
    write(nm,'(a,i3)') 'Gauss', jv;  call rec(nm, Gauss(1.d0, dvs(jv)))
    write(nm,'(a,i3)') 'Loren', jv;  call rec(nm, Loren(1.d0, dvs(jv)))
  end do

  ! --- D02EAF: matrix exponential for a small rate matrix PM(source, dest)
  PM = 0.d0
  do ia = 1, 6
    do ja = 1, 6
      if (ia /= ja) PM(ia,ja) = 1.d12 * dble(mod(3*ia + 5*ja, 7) + 1) * 10.d0**(mod(ia+ja,3))
    end do
  end do
  do ia = 1, 6
    PM(ia,ia) = -sum(PM(ia,:)) + PM(ia,ia)
  end do
  Scale = 1.d40
  dts = (/ 1.d-15, 1.d-13, 2.5d-12 /)
  do it = 1, 3
    yv = 0.d0;  yv(1) = 1.d0
    ifl = -1
    call D02EAF(0.d0, dts(it)*Scale, 6, yv, tolD02, POPdot, WEAF, Nwork, ifl)
    write(nm,'(a,i3)') 'D02_ifail', it;  call reci(nm, ifl)
    do ia = 1, 6
      write(nm,'(a,2i3)') 'D02_y', it, ia;  call rec(nm, yv(ia))
    end do
  end do

  call tclose()
contains
  subroutine sync()   ! FVS* read the channel from the integration context
    cK = k;  cKf = kf;  cJ = j;  cBE = BEk;  cDE = DE
  end subroutine sync
end program test_unit

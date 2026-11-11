! Output helpers for the Code2 tests.
! Every real(8) value is written as its 64-bit pattern (16 hex digits), so the
! comparison with the golden file is bit for bit. Arrays are reduced to an
! order-sensitive, overflow-free bit hash, plus sum and max|x| for diagnosis.
module testio
  implicit none
  integer, save :: uout = -1
contains

  subroutine topen(fname)
    character(*), intent(in) :: fname
    open(newunit=uout, file=fname, status='replace', action='write')
  end subroutine

  subroutine tclose()
    close(uout)
  end subroutine

  function hex(x) result(s)
    real(8), intent(in) :: x
    character(16) :: s
    write(s,'(z16.16)') transfer(x, 0_8)
  end function

  ! one scalar: name, bits, value (value is for reading only, not compared)
  subroutine rec(name, x)
    character(*), intent(in) :: name
    real(8), intent(in) :: x
    write(uout,'(a,1x,a16,1x,es24.16)') trim(name), hex(x), x
  end subroutine

  subroutine reci(name, n)
    character(*), intent(in) :: name
    integer, intent(in) :: n
    write(uout,'(a,1x,i16)') trim(name), n
  end subroutine

  ! bit hash of a real(8) array (any shape, passed as 1D)
  function bhash(a, n) result(h)
    integer, intent(in) :: n
    real(8), intent(in) :: a(n)
    integer(8) :: h
    integer :: i
    h = 1469598103934665603_8
    do i = 1, n
      h = ieor(ishftc(h, 7), transfer(a(i), 0_8))
      h = ieor(h, ishft(h, -29))
    end do
  end function

  subroutine reca(name, a, n)
    character(*), intent(in) :: name
    integer, intent(in) :: n
    real(8), intent(in) :: a(n)
    real(8) :: s, m
    integer :: i
    s = 0.d0; m = 0.d0
    do i = 1, n
      s = s + a(i)
      m = max(m, abs(a(i)))
    end do
    write(uout,'(a,1x,z16.16,1x,a,es24.16,1x,a,es24.16)') trim(name), &
      bhash(a, n), 'sum=', s, 'max=', m
  end subroutine

  subroutine recia(name, ia, n)
    character(*), intent(in) :: name
    integer, intent(in) :: n
    integer, intent(in) :: ia(n)
    real(8) :: a(n)
    a = dble(ia)
    call reca(name, a, n)
  end subroutine
end module testio

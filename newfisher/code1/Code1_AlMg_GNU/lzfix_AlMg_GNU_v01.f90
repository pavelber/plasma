! lzfix_AlMg_GNU_v01.f90
! Make the output files look like those of the old Microsoft Fortran PowerStation (NAG) build.
!
! gfortran prints a leading zero in F and E fields when the field has room for it
! (0.939E-03, 0.0200); the old compiler never did (.939E-03, .0200).  In full-width E
! fields this makes neighbouring numbers run together, e.g.  "3s20.939E-030.000E+00".
! gfortran 16 does not support the Fortran 2023 LZS edit descriptor / LEADING_ZERO=,
! so the files are post-processed once, at the end of the run:
!     "0.ddd"  -> " .ddd"       "-0.ddd" -> " -.ddd"
! A zero directly after a digit or '.' is the tail of another number (10.5) and is kept,
! unless it starts an E-mantissa (exponent of the previous field glued to "0.ddddE+dd").
! "0." without digits after the point (F fields with 0 decimals) is kept, as in the old build.

subroutine FixLeadingZeros()
  implicit none
  integer, parameter :: nf = 16
  character(len=40), parameter :: files(nf) = [character(len=40) :: &
       'DPIs and POPs.dat', 'ZPops.dat', 'POPs_AL.dat', 'POPs_MG.dat', 'MHD.dat', &
       'LineWidth.dat', 'LineList.dat', 'EmiAbsoDetails.dat', 'EffSpInK13inPrintFr.dat', &
       'Powers.dat', 'SpePowFr1.dat', 'SpePowFr2.dat', 'SpePowFr3.dat', &
       'SpePowFr4.dat', 'SpePowFr5.dat', 'SpePowFr6.dat' ]
  integer :: i
  do i = 1, nf
     call FixFileLZ(trim(files(i)))
  end do
end subroutine FixLeadingZeros


subroutine FixFileLZ(fname)
  implicit none
  character(len=*), intent(in) :: fname
  integer, parameter :: maxlen = 1024
  character(len=maxlen), allocatable :: lines(:)
  integer, allocatable :: lens(:)
  character(len=maxlen) :: buf
  logical :: there, isopen
  integer :: u, ios, n, k, sz

  inquire(file=fname, exist=there, opened=isopen, number=u)
  if (.not. there) return
  if (isopen) close(u)                     ! flush what the program wrote

  ! pass 1: count lines
  open(newunit=u, file=fname, status='old', action='read', iostat=ios)
  if (ios /= 0) return
  n = 0
  do
     read(u, '(a)', iostat=ios) buf
     if (ios /= 0) exit
     n = n + 1
  end do
  close(u)
  if (n == 0) return

  ! pass 2: read lines keeping their exact length
  allocate(lines(n), lens(n))
  open(newunit=u, file=fname, status='old', action='read')
  do k = 1, n
     lines(k) = ' '
     read(u, '(a)', advance='no', size=sz, iostat=ios) lines(k)
     lens(k) = sz
     if (ios == 0) read(u, '(a)')          ! line longer than maxlen: rest is dropped (not expected)
     call FixLineLZ(lines(k)(1:lens(k)))
  end do
  close(u)

  open(newunit=u, file=fname, status='replace', action='write')
  do k = 1, n
     write(u, '(a)') lines(k)(1:lens(k))
  end do
  close(u)
  deallocate(lines, lens)
end subroutine FixFileLZ


subroutine FixLineLZ(s)
  implicit none
  character(len=*), intent(inout) :: s
  integer :: i, j, n
  character :: prev
  logical :: is_e

  n = len(s)
  do i = 1, n - 2
     if (s(i:i) /= '0' .or. s(i+1:i+1) /= '.' .or. .not. isdig(s(i+2:i+2))) cycle
     prev = ' '
     if (i > 1) prev = s(i-1:i-1)
     ! does an E-mantissa start here:  0.ddd[EeDd][+-]d ?
     j = i + 2
     do while (j <= n)
        if (.not. isdig(s(j:j))) exit
        j = j + 1
     end do
     is_e = .false.
     if (j + 2 <= n) then
        is_e = index('EeDd', s(j:j)) > 0 .and. index('+-', s(j+1:j+1)) > 0 .and. isdig(s(j+2:j+2))
     end if
     if ((isdig(prev) .or. prev == '.') .and. .not. is_e) cycle   ! tail of a number like 10.5
     if (prev == '-') then
        s(i-1:i) = ' -'
     else
        s(i:i) = ' '
     end if
  end do
contains
  logical function isdig(c)
    character, intent(in) :: c
    isdig = c >= '0' .and. c <= '9'
  end function isdig
end subroutine FixLineLZ

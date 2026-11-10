! lzfix_Co2_GNU.f90
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
  integer, parameter :: nf = 30
  character(len=40), parameter :: files(nf) = [character(len=40) :: &
       'BSinfo.dat', 'CoreInfo.dat', 'CapInfo.dat', 'BSsLineInfo.dat', 'CorLineInfo.dat', &
       'CapLineInfo.dat', 'Contacts.dat', 'EffSpIns.dat', 'ZPopsBS.dat', 'ZPopsCore.dat', &
       'ZPopsCap.dat', 'Frames.dat', 'Image1.dat', 'Image2.dat', 'PowDet1.dat', &
       'PowDet2.dat', 'PowDet3.dat', 'FrameGauLor.dat', 'Happened.dat', 'BSemiAbso.dat', &
       'COREemiAbso.dat', 'CAPemiAbso.dat', 'PIR_BS.dat', 'PIR_Core.dat', 'PIR_Cap.dat', &
       'LineList.dat', 'SpInDi1.dat', 'SpInDi2.dat', 'SpInDi3.dat', 'AlongCeLOS.dat' ]
  integer :: i
  do i = 1, nf
     call FixFileLZ(trim(files(i)))
  end do
end subroutine FixLeadingZeros


subroutine FixFileLZ(fname)
  ! Lines of any length: each line is read in chunks into a growing string,
  ! fixed and written to a temporary file, which then replaces the original.
  implicit none
  character(len=*), intent(in) :: fname
  character(len=:), allocatable :: line
  character(len=4096) :: chunk
  logical :: there, isopen
  integer :: u, v, ios, sz, ios2

  inquire(file=fname, exist=there, opened=isopen, number=u)
  if (.not. there) return
  if (isopen) close(u)                     ! flush what the program wrote

  open(newunit=u, file=fname, status='old', action='read', iostat=ios)
  if (ios /= 0) return
  open(newunit=v, file=fname//'.lztmp', status='replace', action='write', iostat=ios)
  if (ios /= 0) then
     close(u)
     return
  end if
  do
     line = ''
     do
        read(u, '(a)', advance='no', size=sz, iostat=ios) chunk
        line = line // chunk(1:sz)
        if (ios /= 0) exit                 ! end of record (or end of file)
     end do
     if (is_iostat_end(ios) .and. len(line) == 0) exit
     call FixLineLZ(line)
     write(v, '(a)') line
     if (is_iostat_end(ios)) exit          ! last line without a newline
  end do
  close(u)
  close(v)
  open(newunit=u, file=fname, status='old', iostat=ios2)
  if (ios2 == 0) close(u, status='delete')
  call rename(fname//'.lztmp', fname)
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

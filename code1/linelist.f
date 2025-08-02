
      SUBROUTINE LineList()   ! List spectral lines to be seen in [hvSmo, hvMax] and Print hv-regulated LineList in file #30.
	use mo1
	implicit none
	integer lnew, iw, lw, lmin, nXmin ! kSP   ! w-variables
	real(8) hvCmin,	Alamda
	open (30, file='LineList.dat')
      write(30,'(/a30, e7.2, a7,f5.4, a14/)')
     +          	'Lines with A >', AulMin, ', flu >', fluMin,
     +            ',  hvC > hvSmo'

      write(30,'(a81/)') 'XE  SpS     hvC(eV)   Lambda(A)    A(Hz)
     +Upper Level     Lower Level       f'

      linM4= 0
      do nX= 1, nXE         ! counting lines between all ELs of each XE.
        lin=0               ! # of Spectral Line found in [hvSmo, hvmax]
        do k  = 1, NST(nX)  ! Lower state
        do k2 = 2, NST(nX)  ! Upper state
          if(A(k2,k,nX) .LT. 1.) goto 1   ! Weak lines and lines at hv < hvSmo got A=0 in "Intro" subr
          lin = lin +1
          nLoh(lin,nX)= k                 ! Lower level# prior to regulation of the line numbers according to their hvC
          nUph(lin,nX)= k2                ! Upper
          hvCh(lin,nX)= E(k2,nX)- E(k,nX) ! Spectral line center [eV]
  1       continue
        enddo
        enddo
        linM(nX)= lin            ! number of lines in the domain
        linM4= linM4 +linM(nX)   ! same for all XEs
      enddo

      if(linM4.ge.MNLe) then
        write(*,'(a20,i6, a4,i6)') 'Increase MNLe=', MNLe, 'to', linM4
        STOP
      endif

c  Regulation according to "hvC" (thru all XE)
      do lnew= 1, linM4    ! It will be a Line# after regulation along hv (in the common line list for all XEs)
        hvCmin= 1.d7       ! arbitrary value before search
        do nX= 1, nXE
          do lw= 1, linM(nX)                  ! check all lines found in the range
            if(hvCh(lw,nX) .LT. hvCmin) then  ! for finding minimal hvC among all lines of all XEs
              hvCmin= hvCh(lw,nX)             ! smaller hvC found
              lmin= lw              ! remember line# of this smaller hvC
              nXmin= nX             ! remember XE#   of this smaller hvC
            endif
          enddo
        enddo                       ! search completed
        hvC(lnew)= hvCh(lmin,nXmin)
        nLo(lnew)= nLoh(lmin,nXmin)
        nUp(lnew)= nUph(lmin,nXmin)
        nX3(lnew)= nXmin            ! XE# of this line
        hvCh(lmin,nXmin)= 7.d7      ! excludes a possibility to pick up this line again
      enddo

c  Print common (all-XE) line list into file #30
      do iw = 1, linM4
        if(hvC(iw) .lt. hvSmo) STOP '  found Line at hv < hvSmo'
        if(hvC(iw) .gt. hvmax) STOP '  found Line at hv > hvMax'
        Alamda= eVA/hvC(iw)          ! A
        write(30,'(i2, i4, 2f12.3, e11.3, i7, a1, a4,a4, i7, a1, a4, a4,
     +             e12.3)')  nX3(iw), kiSS(nUp(iw),nX3(iw)),
     +       hvC(iw), Alamda, A(nUp(iw),nLo(iw),nX3(iw)), nUp(iw),'=',
     +       QSname1(nUp(iw),nX3(iw)),QSname2(nUp(iw),nX3(iw)), nLo(iw),
     +       '=', QSname1(nLo(iw),nX3(iw)), QSname2(nLo(iw),nX3(iw)),
     +       flu(nLo(iw),nUp(iw),nX3(iw))
      enddo
      linM4= linM4       ! total lines in [hvSmo, hvmax] interval
	close(30)
      Return
      END	   ! of 'LineList' subr


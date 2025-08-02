      subroutine PMg0()  ! For g0 case: transition probability matrix PM(k,kf) and EL depopulation rate Wout(k,nX,La)
      use mo1
      implicit none
c  Total depletion rate of state 'k' into all other states 'kf' is "Wout(k,nX,La)"
      do k = 1, NST(nX)
        Wout(k,nX,La)	= zero     ! depletion rate of EL# k
        if(k .LT. Nnu(nX) .AND.       ! k is non-AI EL
     +     BE(k) .lt. 1.d-3)   cycle  ! k is dead, skip it. Remember that nucl and AIELs are "never dead"
        do kf = 1, NST(nX)            ! to all acceptors
	    if(kf .LT. Nnu(nX) .AND.       ! non-AI, not nucl
     +       BE(kf) .lt. 1.d-3)  cycle   ! skip dead acceptor; only non-AI ELs may be dead.
	    if(kf .ne. k)  Wout(k,nX,La) = Wout(k,nX,La) +    A(k,kf,nX) +
     2             WInd(k,kf) + Wab(k,kf) + WiRR(k,kf) +   WI(k,kf)	   ! Probabs are k --> kf
     3           +  WMI(k,kf) + WTB(k,kf) + WPhI(k,kf) +  WRR(k,kf)
     4           +  WEX(k,kf) + WDX(k,kf) +  WDC(k,kf) + WAiz(k,kf,nX)
        enddo
      enddo

c  Probability matrix PM(k,kf) is a sum over ALL k --> kf channels
      PM= zero
      do k = 1, NST(nX)
	  if(k .LT. Nnu(nX) .AND. BE(k).LT. 1.d-3) cycle  ! "k" is non-AI, "k" is dead';  BE(nucl) ==0.002 eV given in Intro
c                                                       Note: only non-AI ELs may be dead. Nucl and AIELs are "never dead"
        do kf = 1, NST(nX)
	    if(kf .LT. Nnu(nX) .AND. BE(kf).LT. 1.d-3) cycle  ! non-AI "kf", dead "kf", skip dead acceptor; .
	    if(kf .eq. k)  PM(k,k)  = -Wout(k,nX,La)
		if(kf .ne. k)  PM(k,kf) =     A(k,kf,nX)     +
     +               WI(k,kf) +  WMI(k,kf) + WTB(k,kf) + WPhI(k,kf)
     +            + WRR(k,kf) +  WEX(k,kf) + WDX(k,kf) + WAiz(k,kf,nX)
     +            + WDC(k,kf)	+ WInd(k,kf) + Wab(k,kf) + WiRR(k,kf)
        enddo
	enddo
      return
      end subroutine  ! PMg0()


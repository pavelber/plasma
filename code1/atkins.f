      SUBROUTINE AtKins() ! Atomic Kinetics for current La, nX.  On entry POP(EL#) given POPt(k,nX,La) from previous t-step (t=ti)
      use mo1             ! on exit POP(EL#) contains the distribution at t=tf and re-writes POPt(k,nX,La) for current La, nX
      implicit none
      real(8)  POP(NSTm), POPti(NSTm)  ! these files are Local in AtKins
      external POPdot     ! subroutine mentioned by name (no argument) in d02

      CALL redPI()        ! compute Ioniz Potentials for {Te(ti-tf),ni(ti-tf)} of MAIN: PIR(SS,nX)= PI(SS,nX)-dPI(SS,nX)
      CALL BEvPr()        ! compute Binding Energy of ELs

      Sah = Dene*(one-bp)/(6.0371288d21*Te**1.5d0)  ! for 3-body recombination

      do k= 1, NST(nX)
        POP(k) = POPt(k,nX,La)   ! shorter name. POPs from previous t-step
      enddo

CUT-off block
      do k= 1, NST(nX)
        if(POP(k).lt.-1.d-12) STOP 'came to cut-off with POP < -1.d-12'  ! NAG d02 with tolD02= 1.d-6 provides |sumPOP - 1| < 1.e-8 but some small POPs may be negative |POP|<1.e-12
        if( k.LT.Nnu(nX) .AND. BE(k).lt. 1.d-3                           ! dead non-AI EL
     +                   .AND. POP(k).gt. zero) then  ! cut it;  Note: (i) AIELs have BE = -13., I keep them "never dead"
          POP(nuGS(kiSS(k,nX)+1 ,nX)) =
     +    POP(nuGS(kiSS(k,nX)+1 ,nX)) + POP(k)        !  Add it to GS of next SS
          POP(k)= zero                                !  empty cut EL
	  endif
        POPt(k,nX,La) = POP(k)  ! update because "POP" is local here while subr "Ws" uses "POPt(k,nX,La)"
      enddo

Comment on RESTART of dead 'k' level:  when BEp(k,XE,Layer) <0 changes to BE(k)>0, then initial POP(k)==0 is  given proper value in D02

      CALL Ws()  ! calculate all transition probabilities mentioned in Rate Equations
      CALL PMg0  ! PM(k,kf) and Wout(k,nX,La) for g0 case (table ddegeneracy)

      do k= 1, NST(nX)
          POPti(k)= POP(k)
      enddo

c  Integrate the rate equations:
      ifail= -1
      Scale= 1.d40
      tiS= ti*Scale
      tfS= tf*Scale

      CALL D02EAF(tiS, tfS, NST(nX), POP, tolD02, POPdot, WEAF, Nwork,
     +            ifail)

***   After-D02 printout in "Comment.dat". Only in PrFr time:
      if(ng .eq. gpEq(PrFr)) then  ! "ng" is the NUMBER of "tf"-point on t axis
        write(41,'(//a7, f5.3, a20)') 'tf=', tf*1.d9,
     +                                   'ns.  POPs after d02'
        do k= 1, NST(nX)
          if(k.eq.1) write(41,'(/a56)')       'XE  EL#   QSname        E
     +        g0      Wout      POPtf'
          write(41,'(i2, i5, a6,a5,   f11.3, f7.0,    2e11.4)')
     2		       nX, k, QSname1(k,nX), QSname2(k,nX),
     3               E(k,nX), g0(k,nX),     -PM(k,k), POP(k)
        enddo
      endif

check POPs obtained:
      SumP= zero
      do k= 1, NST(nX)
	  if( k.LT.Nnu(nX).AND. BE(k).LT. 1.d-3
     +                  .AND.POP(k).GT. 1.d-15) STOP 'Populated dead EL'
	  if(POP(k) .LT.-1.d-12) Write(*,'(a29,e10.2, a17,i2, i4,e14.6)')  ! NAG d02 with tolD02= 1.d-6 provides |sumPOP - 1| < 1.e-8  but some small POPs may be negative |POP|<1.e-12
     +     'found POP < -1.d-12;  POP=', POP(k),                         ! If these negative values are < -1.e12 we send screen info
     +     'for XE, EL, BE=', nX, k, BE(k)
        sumP= sumP+ POP(k)
      enddo
      if(abs(SumP-one) .gt. 1.d-6) then
         write(*,'(/a20, e9.2)') 'SumPOPs - 1 =', SumP - one
         STOP 'Sum POPs ne 1 after d02'
      endif

calculate POPZ(SS)
      do jSS= FSS(nX), HSS(nX)+1
        POPZ(jSS,nX,La)= zero      ! relative abundance of ionization stage
      enddo
      do k= 1, NST(nX)
        POPZ(KiSS(k,nX),nX,La)= POPZ(KiSS(k,nX),nX,La) + POP(k)
      enddo

      if(nX.eq.1) write(21,'(f7.4, f8.3, 6e11.4)') tf*1.e9, Te/1000.,
     +       POP(PoLeNu(1,1)), POP(PoLeNu(1,2)), POP(PoLeNu(1,3)),	   ! POP of six levels of AL chosen "FLAG.inp".
     +       POP(PoLeNu(1,4)), POP(PoLeNu(1,5)), POP(PoLeNu(1,6))

      if(nX.eq.2) write(22,'(f7.4, f8.3, 6e11.4)') tf*1.e9, Te/1000.,
     +       POP(PoLeNu(2,1)), POP(PoLeNu(2,2)), POP(PoLeNu(2,3)),       ! POP of six levels of AL chosen "FLAG.inp".
     +       POP(PoLeNu(2,4)), POP(PoLeNu(2,5)), POP(PoLeNu(2,6))

c Save 1D arrays POP(k) and BE(k) in 3D arrays for next t-step with this "La" and "XE"
      do k= 1, NST(nX)
         POPt(k,nX,La)= POP(k)
	    BEp(k,nX,La)= BE(k)
      enddo

      Return
      END   ! of 'AtKins' subr


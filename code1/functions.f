      real(8) FUNCTION VOIGT(X,Y) ! Voigt shape normalized to sqrt(pi); From Drayson, JELRT, v.16, pp.611-614, 1976
      use mo1
      implicit none
      integer MAX, N, MIN, Jvo
      real(8) X,Y, B(22), RI(15), XN(15), YN(15), D0(25), D1(25),D2(25),
     +       D3(25), D4(25), HN(25), XX(3),  HH(3), NBY2(19), Cv(21),
     +       CO, UU, VV, U, Y2, Hv, DXv, V
      DATA B/0.,.7093602d-7,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,
     + .0,.0,.0,.0,.0,.0/, XN/10.,9.,2*8.,7.,6.,5.,4.,7*3./,
     + YN/3*.6,.5,2*.4,4*.3,1.,.9,.8,2*.7/, Hv/.201/, XX/.5246476,
     + 1.65068,.7071068/, HH/.2562121,.2588268E-1,.2820948/, NBY2/9.5,
     + 9.,8.5,8.,7.5,7.,6.5,6.,5.5,5.,4.5,4.,3.5,3.,2.5,2.,1.5,1.,.5/,
     + Cv/.7093602E-7,-.2518434E-6,.8566874E-6,-.2787638E-5,.866074E-5,
     +  -.2565551E-4,.7228775E-4,-.1933631E-3,.4899520E-3, -.001173267,
     + .2648762E-2, -.005623190, .01119601, -.02084976, .03621573,
     + -.05851412, .08770816, -.121664, .15584, -.184, .2/
      DO I= 1,15
        RI(I)=-I/2.
      ENDDO
      DO I=1,25
        HN(I)=Hv*(I-.5)
        CO=4.*HN(I)*HN(I)/25.-2.
        DO Jvo =2,21
          B(Jvo+1)=CO*B(Jvo)-B(Jvo-1)+Cv(Jvo)
        ENDDO
        D0(I)=HN(I)*(B(22)-B(21))/5.
        D1(I)=1.-2.*HN(I)*D0(I)
        D2(I)=(HN(I)*D1(I)+D0(I))/RI(2)
        D3(I)=(HN(I)*D2(I)+D1(I))/RI(3)
        D4(I)=(HN(I)*D3(I)+D2(I))/RI(4)
      ENDDO
104   IF (X-5.) 105,112,112
105   IF (Y-1.) 110,110,106
106   IF (X.GT.1.85*(3.6-Y)) GO TO 112

      IF (Y.LT.1.45) GO TO 107
      I=Y+Y
      GO TO 108
107   I=11.*Y
108   Jvo=X+X+1.85
      MAX= XN(Jvo)*YN(I)+.46
      MIN= MIN0(16,21-2*MAX)

      UU=Y
      VV=X
      DO Jvo=MIN,19
        U=NBY2(Jvo)/(UU*UU+VV*VV)
        UU=Y+U*UU
        VV=X-U*VV
      ENDDO
      VOIGT=UU/(UU*UU+VV*VV)/1.772454
      RETURN
110   Y2=Y*Y
      IF (X+Y.GE.5.) GO TO 113

      N=X/Hv
      DXv=X-HN(N+1)
      U=(((D4(N+1)*DXv+D3(N+1))*DXv+D2(N+1))*DXv+D1(N+1))*DXv+D0(N+1)
      V=1.-2.*X*U

      VV=DEXP(Y2-X*X)*DCOS(2.*X*Y)/1.128379-Y*V
      UU=-Y
      MAX=5.+(12.5-X)*.8*Y
      DO I=2,MAX,2
        U=(X*V+U)/RI(I)
        V=(X*U+V)/RI(I+1)
        UU=-UU*Y2
        VV=VV+V*UU
      ENDDO
      VOIGT=1.128379*VV
      RETURN
112   Y2=Y*Y
      IF (Y.LT.11.-.6875*X) GO TO 113

      U=X-XX(3)
      V=X+XX(3)
      VOIGT=Y*(HH(3)/(Y2+U*U)+HH(3)/(Y2+V*V))
      RETURN

113   U=X-XX(1)
      V=X+XX(1)
      UU=X-XX(2)
      VV=X+XX(2)
      VOIGT=Y*(HH(1)/(Y2+U*U)+HH(1)/(Y2+V*V)+HH(2)/(Y2+UU*UU)+HH(2)/(Y2+
     1VV*VV))
      RETURN
      END     ! FUNCTION VOIGT



      real(8) FUNCTION Gauss(fwG, devW)  ! Gauss shape: p(v)dv= (dv/GaPa/sqpi)*exp(-[{v-v0}/GaPa]^2)
      use mo1                            ! has FWHM= 2*sqrt[ln(2)]*GaPa= 1.66510922*GaPa;  Vains, p.248; Griem3 p.54
      implicit none                      ! where GAuss PArameter "GaPa" == FWHM/1.66510922
      real(8) fwG, devW, GaPa
      Gauss= zero
      GaPa = fwG/1.66510922d0
      Gauss= dexp(-(devW/GaPa)**2)/GaPa/sqpi  ! Gaussian of known FWHM
      END



      real(8) FUNCTION Voigt2(fwhmL, fwhmG, vC, dv) ! [eV], dv= v-vC, Sasha's connector to JELRT Drayson's VOIGT(X,Y); WELL checked
	use mo1
	implicit none
	real(8) fwhmL, fwhmG, vC, dv, balf, x1, y1, Voi2, VOIGT
      balf= (vC/fwhmG)*1.6651092   ! 1.6651092= 2*Sq[ln(2)]
      x1= balf*abs(dv)/vC	         ! to Dr' units
      y1= 1.665109*fwhmL/2./fwhmG  ! to Dr' units
      Voi2= VOIGT(x1,y1)/1.772454  ! with renorm from "sqrt(pi)=1.7724539; Drayson" to 1
      Voigt2= Voi2*balf/vC	     ! to "eV"
      END

      real(8) function FVSDx(ev)   ! In notations of excitation: (j,k=Lower) --> (j, kf>k)
	use mo1
	implicit none
	real(8) ev, V, SigDx, SigExc , EED
      SigDx= SigExc(ev+DE)*(ev+DE)*g0(k,nX)/g0(kf,nX)/ev   ! Klein-Rosseland, SVYu95, p5
      V = 5.930968d7*sqrt(ev)
      FVSDx = EED(ev) * V * SigDx
      END


      real(8) function SigExc(eeV)  ! 'k' is lower,  'kf' is upper
	use mo1
	implicit none
	integer Me
      real(8) eeV, As, Bs, Cs, Ds, Es, fReg, X, X2, Sig, Gaunt
      As= Ax(k,kf,nX)
      Bs= Bx(k,kf,nX)
      Cs= Cx(k,kf,nX)
      Ds= Dx(k,kf,nX)
      Es= Ex(k,kf,nX)
c     Fs= Fx(k,kf,nX)      ! 5th coef not used im methods 0, 5, 11
	Me=	MthdEX(k,kf,nX)
      fReg = fVR(k,kf,nX)
      if(DE.LE.zero) then
        write(*,'(a40, 3i5,f15.4)') 'DE <= 0 in SigExc; XE, k, kf, DE=',
     +                                                  nX, k, kf, DE
        STOP
      endif

      X= eeV/DE
      if(X.LT.1.) then
        write(*,'(a40, 3i5, f9.4)') 'X < 1 in SigExc; XE, k, kf, DE=',
     +                                                nX, k, kf, DE
        STOP
      endif
      X2= X*X

      SELECT CASE (Me)        ! # for fitting formula selection
      Case(0)                 ! Van Regemoter;  "ExcXE...inp" contains "f Regemor" > 0 for this (k,kf) couple.
        if(fReg.lt.zero) then
	    write(*,'(a35, 4i5)')'Came to VR with fVR < 0; XE, j, k, kf=',
     +                                                   nX, j, k, kf
		STOP
        endif
        if(j.EQ.1 .and. nprin(k,nX).EQ.nprin(kf,nX))     ! "Dn = 0" transitions in atoms
     +              Gaunt= (.33-.3/X + .08/X2)*Dlog(X)   !   our Go (4) for atoms
        if(j.EQ.1 .and. nprin(k,nX).NE.nprin(kf,nX))     ! "Dn > 0" transitions in atoms
     +              Gaunt= (.276- .18/X )*Dlog(X)        !   our G> (6) for atoms
        if(j.GT.1 ) Gaunt= .349*Dlog(X)+ .0988+ .455/X   ! Ions. Actually, for "n --> n'" in H-like ions (5)
        Sig = Ampli *fReg *Gaunt /X/DE/DE

      Case( 5)
        Sig= As/X +Bs/X2 +Cs/X/X2 +Ds/X2/X2 +Es*Dlog(X)/X

      Case(11)                                           ! in CODA SGM=(C+B*X1+A*X12)/(X1+D)**4/X1**E
	  Sig= (Cs +Bs*X +As*X2)/(X+Ds)**4 /X**Es
      END SELECT

      if(Sig .lt. zero) Sig= zero  ! bad fits to FAC points can result in Sig < 0 even far from threshold
      SigExc= Sig
      END


	real(8) function SigPhi(hv)  ! cm2, eV;  PhotoIonz Cross-Sec.   Notations:  (j,k) + hv --> (j+1,kf) + e
      use mo1
	implicit none
	real(8) hv, xw
	SigPhi= zero
  	if(hv .lt. BEk) STOP 'Came in SigPhi with hv < BEk'
      xw = hv/BEk
      SigPhi= (Eix(k,kf,nX)+ Fix(k,kf,nX)/xw + Gix(k,kf,nX)/xw/xw) / 	    ! MFGu fit to SigPhi(x), see "GU expres for Cross-sec.pdf"
     +                                         xw**(3.5+Hix(k,kf,nX))
      if(SigPhi.lt.zero) SigPhi= zero  ! Bad fits to FAC points may give Sig < 0 even far from threshold
      END

      real(8) function FVSEx(ev)
	implicit none
	real(8) ev, V, EED, SigExc
        V= 5.930968d7*sqrt(ev)
        FVSEx= EED(ev)* V* SigExc(ev)
      END




      real(8) function FVStbr(eeV)   ! In ionization we used Notations: (j,k) + e = (j+1,kf) +2e
	use mo1
	implicit none                  ! Our "Non-Maxw EED" is Maxw+Beam. High-E electrons do not recombine, therefore 'recombining el density'= Dene*(1-bp)
	real(8) eeV, SigTbr, V, EED, SigInz
      SigTbr= Sah* SigInz(eeV+BEk)*(eeV+BEk)*g0(k,nX)/g0(kf,nX)/eeV  ! demanding detailed balance in partial (Maxw-Bolt-Saha) LTE; tCRC pap
      V = 5.930968d7*sqrt(eeV)
      FVStbr= EED(eeV) * V * SigTbr
      END


      real(8) function FVSrr(eeV)
      implicit none
      real(8) eeV, V, EED, SigRR
      V= 5.930968d7*sqrt(eeV)
      FVSrr = EED(eeV) *V *SigRR(eeV)
      END


      real(8) function SigRR(eeV)   ! Notations in ionization where (SS=j, k) + hv --> (SS=j+1, kf) + e
      use mo1
      implicit none
      integer ksave, kfsave
      real(8) eeV, hv, SigPhI

      hv= eeV + BEk
      SigRR= SigPhI(hv)*g0(k,nX)*hv**2/g0(kf,nX)/eeV/1.0220d6   ! BlackFold (39); 1.022 MeV= 2mc^2

      if(ReverseKKF .GT.1.) then  ! In FB Emission notations are (SS=j+1,k) + e  --> (j, kf) + hv
c                                 Therefore for correct computation of "SigRR" I must reverse k,kf
         ksave = k
         kfsave= kf
         k = kf         ! like-in-ioniz notations (j,k) + hv --> (j+1,kf) for calling func "SigRR"
         kf= ksave
         SigRR= SigPhI(hv)*g0(k,nX)*hv**2/g0(kf,nX)/eeV/1.0220d6
         k = ksave      ! returm sense of k,kf in FB emi
         kf= kfsave
         ReverseKKF = 0.
      endif

      if(SigRR.lt.zero) then
        write(*,'(a33, i2, i3, 2i4, 3f9.2, e10.2)')
     +   'SigRR < 0 for XE, j, k, kf, BEk, hv, eeV, SigPhi =',
     +                  nX, j, k, kf, BEk, hv, eeV, SigPhI(hv)
        STOP '  2025'
      endif
      END




      real(8) function EED(eeV)   ! Electron energy distribution.
	use mo1                     ! Note: bp=0 results in Maxwellian
      implicit none
	real(8) eeV, EED2, EED1
      EED2= zero
      if((bc-bw/2.) .lt. eeV .and. eeV .lt. (bc+bw/2.))
     +	           EED2= 1./bw
      EED1= 2.*sqrt(eeV/pin/Te)*exp(-eeV/Te)/Te    ! LL StatPhys p 108
      EED= (one- bp)*EED1 + bp*EED2
      END


      real(8) function FVSinz(eeV)
	use mo1
	implicit none
	real(8) eeV, V, EED, SigInz
         ! Safety check to prevent sqrt of negative values
         if(eeV .LE. 0.d0) then
            FVSinz = 0.d0
            return
         endif
         V= 5.930968d7*sqrt(eeV)          ! cm/s,  sqrt(2kE/m)
         FVSinz= EED(eeV)*V*SigInz(eeV)
      END


      real(8) function SigInz(eeV)     !  (SS=j, k) + e = (j+1, kf) +2e
	use mo1
	implicit none
	real(8) eeV, xw, yw, OM  ! OM is MFGu Collision Strength (Omega)
      SigInz= zero
      if(eeV.lt.BEk) then
         write(*,*) 'Warning: SigInz called with E < BEk:', eeV, BEk
         return
      endif

      ! Add safety check for very small energy differences
      if(eeV .LE. BEk*1.001d0) then
         SigInz = zero
         return
      endif

      xw= eeV/BEk

      ! Check for numerical issues
      if(xw .LE. 1.d0 .OR. xw .NE. xw) then
         SigInz = zero
         return
      endif

      yw= one - one/xw  ! MF Gu

      ! Check for potential numerical issues in log
      if(xw .LE. 1.d-10 .OR. xw .GE. 1.d10) then
         SigInz = zero
         return
      endif

      OM = Aix(k,kf,nX)*Dlog(xw)+ Bix(k,kf,nX)*yw*yw +   ! FAC guide (2.9)
     +     Cix(k,kf,nX)*yw/xw   + Dix(k,kf,nX)*yw/xw/xw

      ! Check if OM is reasonable
      if(OM .NE. OM .OR. abs(OM) .GE. 1.d20) then
         SigInz = zero
         return
      endif

      SigInz= 3.8101e-16* OM /eeV /g0(k,nX)              ! FAC guide (2.10): "in A.U. e-imp SigInz= OM/k0^2/g(k)".
c                                                          A.U. [Sig]= a0^2=2.8003(-17) cm2.
      if(SigInz .lt. zero)   SigInz= zero      ! Bad fits to FAC points may have Sig < 0 even far from threshold

      ! Final sanity check
      if(SigInz .NE. SigInz .OR. abs(SigInz) .GE. 1.d20) then
         SigInz = zero
      endif
      END



      integer function char2int(symbol)
	implicit none
      character*1 symbol
        SELECT CASE (symbol)
         CASE (' ')
           char2int = 1
         CASE ('0':'9')
           char2int = ichar(symbol) - 48
         CASE ('s')
           char2int = 0
         CASE ('p')
           char2int = 1
	   CASE ('d')
	     char2int = 2
	   CASE ('f')
           char2int = 3
         CASE ('g')
           char2int = 4
         CASE ('h')
           char2int = 5
         CASE ('n')
           char2int = 0
         CASE ('l')	  ! in 4l AIELs of [He], say neon
           char2int = 0
         CASE DEFAULT
           WRITE (*,*) symbol, ' not convertible into integer'
           STOP 'STOPped in function char2int'
        END SELECT
      END







      real(8) function K13(kR)   !  for uniform spherical plasma
      use mo1
      implicit none
      real(8) kR
      K13= Ksp(1)      ! remains for kR <= kRp(1)
      do i= 2, K13po
        if(kR.gt.kRp(i-1).and.kR.le.kRp(i)) then
	   K13= Ksp(i-1)+(Ksp(i)-Ksp(i-1))*(kR-kRp(i-1))/(kRp(i)-kRp(i-1))
         goto 1
        endif
      enddo
      if(kR.gt.kRp(K13po)) K13= Ksp(K13po)
  1   continue
      end

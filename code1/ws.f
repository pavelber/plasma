       SUBROUTINE Ws()  ! Transition probabilities for Rate Equations.
      use mo1          ! In each W-matrix, first index shows INITIAL EL of the transition.
      implicit none
      integer  lUL, lw, kAI
      real(8)  pv(nvM), freq, Wave, DEJ, BemHz, Bem, Bab, hvCw,
     +         AbSpIn, up, rate, prob, Ejm, wFac,
     +         SigRR, V, fun, SigPhi, eeV, dev, Voigt2,
     +         FWevGau, EED, Sca, ArPv
      external FVSinz, FVSmi, FVStbr, FVSrr, FVSex, FVSDx
      real(8)  epsabs, epsrel, result, abserr
      integer  ier, neval
      logical  debug_quad

      debug_quad = .true.  ! Enable debugging to find problem

      write(*,'(a,f12.3)') 'Ws: Starting with Te =', Te
      if(bp.gt.1.d-7) then
         write(*,'(a,3f12.3)') 'Ws: bp, bc, bw =', bp, bc, bw
      endif

      WI  = zero
      WMI = zero
      WEX = zero
      WDX = zero
      WRR = zero
      WTB = zero
      WInd= zero
      Wab = zero
      WPhI= zero
      WDC = zero
      WiRR= zero

*  Probability of bound-bound INDUCED emission and absorption: Wind(U,L) and Wab(L,U)
      do kL= 1, NST(nX)-1
      do kU= kL+1, NST(nX)
        if(A(kU,kL,nX) .LT. 1.)      goto 2  ! this kU,kL couple does non play: Weak lines and near-soft-edge lines got A==0 in Intro
        if(kU.LT.Nnu(nX) .AND.               ! non-AI EL,
     +     BE(kU).LT.1d-3)           goto 2  ! dead; Note: AIELs are treated as "never-dead"; they have no true BE, thus carry initial BE = -13
        if(POPt(kU,nX,La).LT.1.d-12) goto 2  ! dead of poor kU, out of interest
        DE= E(kU,nX) - E(kL,nX)              ! photon energy, eV
        if(DE .LT. hvSmo)            goto 2  ! small-hv lines are (i) broad by Life-time, (ii) not seen above FF continuum
        if(DE .GT. hvMax*0.9)        goto 2

        freq= DE/h       ! photon frequency, Hz
        Wave= c/freq     ! cm
        DEJ = BolJ* DE   ! photon energy, J
c  'Bem' is Einstein B coef for induced emission in kU --> kL
        BemHz= A(kU,kL,nX)*Wave**2/two/DEJ   ! Hz*cm2*sr/J,  A*Lam^2/2hv(J), eq (6) tCRC PAP
        Bem= BemHz*h                         ! eV*cm2*sr/J;  units for * SpIn [W/sr/eV/cm2] --> [W/J]=1/s
c  'Bab' is Einstein B coef for absorption in kL --> kU line
        Bab= Bem*g0(kU,nX)/g0(kL,nX)         ! eq (7) tCRC PAP
c  'Wind(U,L)' and 'Wab(L,U)' are  B * integ{dv*p(v)*SpInEff(v)};  'p(v)' is kL-->kU absorption line shape
        lUL= -3                     ! # of kU-->kL line in the LineList
        do lw= 1, linM4             ! catch this "lUL"
           hvCw= hvC(lw)
           dev= abs(DE-hvCw)/DE
           if(dev .lt. 1.d-7) then
              lUL= lw
              goto 1
           endif
         enddo
         write(*,'(a22, 2i5, 2f10.4)') 'kL, kU, DE, hvC=',
     +                                  kL, kU, DE, hvC(lw)
         STOP 'Did not recognize the Line'
 1       continue
         if (lUL .lt. 1) then        ! not found
            write(41,'(a40, 3i5, f11.4, e11.3)')
     +      'lUL < 1 for XE, kU, kL, DE, A =', nX, kU, kL, DE,
     +                                             A(kU,kL,nX)
            STOP 'Line# not found'
         endif

         if(FWevLor(lUL) .LT. 1.d-16) goto 2  ! "FWevNatu" =0 for dead levels becouse their "Wout(kU,nX,La)"==0
c                                               "StJen" exist only for hv > 1200 eV
c                                     Thus many lines are formally present but their U is dead and FWevLor(lw)=0
         FWevGau= hvCw *FWkVcGAU(nX)  ! Gaussian FWHM [eV] of kU-->kL line of XE# = nX
         Wing   = hvCw/2.             ! Line Wing Length [eV]
         ArPv   = zero
         do iv= 1, nvM
            dev= abs(hvV(iv) - hvCw)
            pv(iv)= zero
            if(hvV(iv).GT. hvCw-Wing .and.
     +         hvV(iv).LT. hvCw+Wing      ) then
               pv(iv)= Voigt2(FWevLor(lUL), FWevGau, hvCw, dev)    ! JELRT (Sawa) Voigt is OK to 1% for any L,G, but slower than wrong "VoigtJS"
            endif
            if(iv.GT.1) ArPv = ArPv +                              ! Area under shape < 1 because line wings cut thus part of photons lost.
     +       	          0.5*(pv(iv)+pv(iv-1))*(hvV(iv)-hvV(iv-1))  ! To return them, no-wings shape will be re-norm to Integ[pBB(v)dv] = 1
         enddo                                                     ! see "/ArPv" below

         if(ArPv .LT.1.d-5) then
            write(*,'(a26, i6, f10.3, 9e10.2)')
     +      'lw, hvC, FWevGau, ArPv =',
     +       lw, hvC(lw), FWevGau, ArPv
          STOP '  ArPv < d-5)'
         endif

         AbSpIn = zero  ! v-integral of {dv*p(v)*SpInEff(v)} [W/eV/cm2/sr]
         do iv= 2, nvM
            dhv= hvV(iv)-hvV(iv-1)
            AbSpIn= AbSpIn + dhv*	SpInEff(La,iv)*pv(iv)/ArPv ! "ArPv" renorms no-wings under-shape areq to 1.
         enddo
         WInd(kU,kL)= Bem*AbSpIn
         Wab (kL,kU)= Bab*AbSpIn
 2       continue
      enddo
      enddo

*  Ionization (removal of ONE electron) probability matrixes WI(k,kf), WPhI(k,kf) and backward WTB(kf,k), WRR(kf,k), WiRR(kf,k) .
*
      up= 100.*Te                              ! Notations: (j,k) + e = (j+1,kf) +2e
      if(bp.gt.1.d-7) up= 100.*Te +bc +6.*bw

      write(*,'(a,2f12.3)') 'Ws: Initial up calculation, Te, up =',
     +   Te, up

      do   k  = 1, LastAI(nX)
        do kf = 1, LastAI(nX)
           if(bra(k,kf,nX) .LT. 0.5) cycle  ! no link;  "bra" is 0 or 1 with FAC bases
           if(k .LT. Nnu(nX) .AND.          ! non-AI EL,
     +        BE(k).LT.1d-3)        cycle  ! this EL is dead; Note 1: Nucl given BE= 0.002 eV; Note 2: Nucl and AIELs cannot be dead
           if(kf.LT. Nnu(nX) .AND.          ! non-AI EL,
     +        BE(kf).lt.1.d-3)       cycle  ! this EL is dead; leave WI(k,kf)= 0
           j = KiSS(k,nX)
           BEk= Eth(k,kf,nX)-DPI(j,nX)      ! energy for "(j,k) + e = (j+1,kf) +2e" ionization
           if(BEk .GE. 0.99*up)      cycle  ! "Te" too low, see limits of d01 integral below

           if(k.GT.Nnu(nX)   .AND.          ! AI EL. Because of big DPI this k=AI --> kf ionization
     +        BEk .LT. zero) BEk= 1.        ! does not require E, but I give formal 1 eV to avoid 0.
c                                           Note: "BEk < 0" never happens for non-AI k because big DPI cuts it.

           if(debug_quad .and. (k.eq.1 .and. kf.eq.599)) then
              write(*,'(a,2i4,4f12.3)')
     +         'Before bounds check k,kf,BEk,up,Eth,DPI:',
     +             k, kf, BEk, up, Eth(k,kf,nX), DPI(j,nX)
           endif

           ! Check for valid integration bounds and parameters
           if(BEk.ne.BEk .or. up.ne.up .or. BEk.ge.up) then
              if(debug_quad) then
                 write(*,'(a,2i4,3e12.4)') 'Invalid bounds:',
     +                k, kf, BEk, up, BEk-up
              endif
              goto 10
           endif

           ! More aggressive bounds checking - prevent extreme values
           if(up .GT. 10000.d0) then
              write(*,'(a,2i4,2f12.3)') 'Capping extreme up value:',
     +             k, kf, up, 10000.d0
              up = 10000.d0  ! Cap at 10 keV instead of 1 MeV
           endif
           if(BEk .LT. 0.1d0) then
              BEk = 0.1d0  ! Minimum 0.1 eV instead of micro-eV
           endif
           if(BEk .GE. up*0.95) then
              write(*,'(a,2i4,2f12.3)')
     +        'Skipping - BEk too close to up:',
     +             k, kf, BEk, up
              goto 10
           endif

           epsabs= 1.d-6  ! Less strict tolerance
           epsrel= 1.d-4  ! Less strict tolerance

           if(debug_quad) then
              write(*,'(a,2i4,2e12.4)') 'DQNG FVSinz:',
     +             k, kf, BEk, up
           endif

           ! Add try-catch equivalent for floating point errors
           rate = zero
           result = zero
           abserr = zero
           ier = -1

           ! Test if function is well-behaved at endpoints
           if(debug_quad .and. (k.eq.1 .and. kf.eq.599)) then
              write(*,'(a)')
     +      'Testing function at bounds before integration'
           endif

           ! Additional safety check before integration
           if(BEk .LE. 0.d0 .OR. up .LE. BEk .OR.
     +        BEk .NE. BEk .OR. up .NE. up) then
              if(debug_quad) then
                 write(*,'(a,2i4,2e12.4)')
     +            'Invalid integration bounds:',
     +                k, kf, BEk, up
              endif
              goto 10
           endif

           ! Additional check: ensure reasonable bounds ratio
           if(up/BEk .LT. 1.01d0) then
              if(debug_quad) then
                 write(*,'(a,2i4,2e12.4,f8.3)')
     +            'Bounds too close, ratio:',
     +                k, kf, BEk, up, up/BEk
              endif
              goto 10
           endif

           CALL DQNG(FVSinz, BEk, up, epsabs, epsrel,
     +                result, abserr, neval, ier)

           if(debug_quad .and. ier.ne.0) then
              write(*,'(a,3i4,2e12.4)') 'DQNG FVSinz error:',
     +             k, kf, ier, result, abserr
           endif

           if(ier.eq.0
     +     .and. result.eq.result
     +     .and. abs(result).lt.1.d20) then
              rate = result
           else
              if(debug_quad) then
                 write(*,'(a,2i4,i4,2e12.4)') 'Bad result:',
     +                k, kf, ier, result, abserr
              endif
              rate = zero
           endif
           if(rate.ne.rate .or. abs(rate).ge.1.d20) rate = zero  ! NaN/inf check
           WI(k,kf)= rate * Dene

10         continue

           ! Fix the second DQNG call with proper bounds checking
           rate = zero
           result = zero
           abserr = zero
           ier = -1

           ! Check bounds for FVStbr integration
           if(up .GT. 1.d-6 .AND. up .LT. 1.d20) then
              CALL DQNG(FVStbr, 1.d-6, up, epsabs, epsrel,
     +                   result, abserr, neval, ier)
              if(ier.eq.0 .and. result.eq.result .and.
     +           abs(result).lt.1.d20) then
                 rate = result
              endif
           endif
           if(rate.ne.rate .or. abs(rate).ge.1.d20) rate = zero
           WTB(kf,k)= rate * Dene

           prob = 0.
           do iv= 2, nvM
             if(hvV(iv).GE. BEk) then
               dhv = hvV(iv) - hvV(iv-1)
               fun= FoPi*SigPhi(hvV(iv))*SpInEff(La,iv)/hvV(iv)
     +              /BolJ
               prob= prob + dhv * fun
             endif
           enddo
           WPhI(k,kf)= prob

           ! Fix third DQNG call with proper bounds checking
           rate = zero
           result = zero
           abserr = zero
           ier = -1

           if(up .GT. 1.d-6 .AND. up .LT. 1.d20) then
              CALL DQNG(FVSrr, 1.d-6, up, epsabs, epsrel,
     +                   result, abserr, neval, ier)
              if(ier.eq.0 .and. result.eq.result .and.
     +           abs(result).lt.1.d20) then
                 rate = result
              endif
           endif
           if(rate.ne.rate .or. abs(rate).ge.1.d20) rate = zero
           WRR(kf,k)= rate * Dene

           prob= zero
           do iv = 2, nvM
             eeV = hvV(iv) - BEk
             if(eeV.gt.0.) then
                V= 5.930968d7*sqrt(eeV)
                Sca= 5040.357* hvV(iv)**3
                fun= V*EED(eeV)*SigRR(eeV)*SpInEff(La,iv)/Sca
                dhv = hvV(iv) - hvV(iv-1)
                prob= prob+ fun *dhv
             endif
           enddo
           WiRR(kf,k) = prob *Dene
        enddo
      enddo

*  Excitation Probability matrix  WEX(k,kf) and backward De-excitation WDX(kf,k).  Notations: (j,k=Lower) --> (j, kf>k)
      do j = FSS(nX), HSS(nX)
        do k = nuGS(j,nX), nuGS(j+1,nX) -1    ! Lower level is non-AI EL of "j";
          if(BE(k) .LT. 1.d-3)  cycle         ! skip dead lower; leave WEX(k,kf)=0
          do kf= k+1,   NST(nX)               ! thru ELs above "k", part of them have another ion charge
            if(mthdEX(k,kf,nX).eq.-7) cycle   ! "-7" means that transition not described in "ExcXE...inp":  leave WEX=0
            if(kf.LT.Nnu(nX) .AND.            ! true EL has BE
     +         BE(kf).LT.1.d-3)    cycle      ! skip dead candidate for Upper; leave WEX(k,kf)=0
            DE= E(kf,nX) - E(k,nX)            ! as both belong to same "j", thus have common GrSt
            if(DE.le.0.) STOP 'DE<=0 in Ex1'  ! cannot happen because we check and correct "ELsXE...inp" in "Intro" subr. Later distorsion never observed.
            if(DE.GE.up) cycle                ! gap too large for Te(ti)
            epsabs= 1.d-8
            epsrel= 1.d-6

            ! Add bounds checking for FVSEx integration
            rate = zero
            result = zero
            abserr = zero
            ier = -1

            if(DE .GT. 0.d0 .AND. up .GT. DE .AND. up .LT. 1.d20) then
               CALL DQNG(FVSEx, DE, up, epsabs, epsrel,
     +                    result, abserr, neval, ier)
               if(ier.eq.0 .and. result.eq.result .and.
     +            abs(result).lt.1.d20) then
                  rate = result
               endif
            endif
            if(rate.ne.rate .or. abs(rate).ge.1.d20) rate = zero
            WEX(k,kf)= rate * Dene

            ! Add bounds checking for FVSDx integration
            rate = zero
            result = zero
            abserr = zero
            ier = -1

            if(up .GT. 1.d-6 .AND. up .LT. 1.d20) then
               CALL DQNG(FVSDx, 1.d-6, up, epsabs, epsrel,
     +                    result, abserr, neval, ier)
               if(ier.eq.0 .and. result.eq.result .and.
     +            abs(result).lt.1.d20) then
                  rate = result
               endif
            endif
            if(rate.ne.rate .or. abs(rate).ge.1.d20) rate = zero
            WDX(kf,k)= rate * Dene
          enddo
        enddo
        if(nuAS(j,nX) .GT. 0) then
          do k = kAI1(j,nX), kAI2(j,nX)-1  ! Lower level is AI EL of "j"; the last AIEL cannot be excited
            do kf= k+1, kAI2(j,nX)         ! Upper level must be AI(j) also; Note: (i) AIELs are treated as ,
c                                              "never-dead", (ii) they have no true BE, they carry initial BE= -13
              if(mthdEX(k,kf,nX).eq.-7) cycle  ! "-7" means that transition not described in "ExcitXE.inp":  leave WEX=0
              DE= E(kf,nX) - E(k,nX)           ! as both belong to same "j", thus have common GS
              if(DE.GE.up)  cycle
              epsabs= 1.d-8
              epsrel= 1.d-6

              ! Add bounds checking for FVSEx integration
              rate = zero
              result = zero
              abserr = zero
              ier = -1

              if(DE .GT. 0.d0 .AND. up .GT. DE .AND.
     +           up .LT. 1.d20) then
                 CALL DQNG(FVSEx, DE, up, epsabs, epsrel,
     +                      result, abserr, neval, ier)
                 if(ier.eq.0 .and. result.eq.result .and.
     +              abs(result).lt.1.d20) then
                    rate = result
                 endif
              endif
              if(rate.ne.rate .or. abs(rate).ge.1.d20) rate = zero
              WEX(k,kf)= rate * Dene

              ! Add bounds checking for FVSDx integration
              rate = zero
              result = zero
              abserr = zero
              ier = -1

              if(up .GT. 1.d-6 .AND. up .LT. 1.d20) then
                 CALL DQNG(FVSDx, 1.d-6, up, epsabs, epsrel,
     +                      result, abserr, neval, ier)
                 if(ier.eq.0 .and. result.eq.result .and.
     +              abs(result).lt.1.d20) then
                    rate = result
                 endif
              endif
              if(rate.ne.rate .or. abs(rate).ge.1.d20) rate = zero
              WDX(kf,k)= rate * Dene
            enddo
          enddo
        endif
      enddo



* Dielectronic Capture: (j,k1=reg) + e --> (j-1,kAI).  We follow Griem3 (6.35), i.e. derive WDC from detailed balance with WAiz in high-density limit.
c                                                      Algorithm, used for "Ejm" and "WDC", is checked by exact (in LTE) Griem3 (6.34) formula
c                                                            WDC(k1,kAI) = WAiz(kAI,k1)*popLTE(kAI)/popLTE(k1).
c                                       tCR comparison showed "==" in all POPs (but at that time we used "PI", not "PIR" for Ejm).
      do j = FSS(nX)+1, HSS(nX)       ! capturing ion
        if(nuAS(j-1,nX).lt.1) goto 9       ! no AI ELs in SS=j-1
        do k1 = nuGS(j,nX), nuGS(j+1,nX)-1    ! candidates for capturing EL, they are non-AI ELs
          if(BE(k1) .LT. 1.d-3)  cycle        ! dead EL
          do kAI= kAI1(j-1,nX), kAI2(j-1,nX)  ! AI ELs of SS=j-1.  Note: (i) AIELs are treated as "never-dead",
c                                                                        (ii) they have no true BE, they carry initial BE= -1
            if(WAiz(kAI,k1,nX) .gt. zero) then       ! this (kAI,k1) couple has DC/AI channel
              Ejm= E(kAI,nX) -PIR(j-1,nX) -E(k1,nX)  ! General case. Griem3 "Ejm" in p.178.
              wFac= 4.*(0.88d-16*13.6/Te)**1.5
              rate= wFac* (g0(kAI,nX)/g0(k1,nX))
     +            * WAiz(kAI,k1,nX) *dexp(-Ejm/Te)   ! (6.35)
              WDC(k1,kAI) = Dene * rate              ! (6.34)
            endif
          enddo     ! kAI
        enddo       ! k1
  9     continue
      enddo         ! j
      return
      END    ! of 'Ws' subr
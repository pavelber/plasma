       SUBROUTINE EmiAbso()
       use mo1
       implicit none
C      Include the shared variable declarations
C      INCLUDE 'globals.inc'

C      Local variables for this subroutine only
       integer lw
       real(8) EmiLFul(MNLe), AbsoAm(MNLe), AlfaBB(MNLe)
       real(8) absorBB(nvM),  emisBB(nvM),
     +        absorFF(nvM),  emisFF(nvM), pBB(nvM), ArPBB(MNLe),
     +        DiZ2, FrE,  dev, FWevGau, HaLor, EED,
     +        SigRR, Vel, hvJ, contrib,  alfaFB, Voigt2,      ! Voigt2 is Sawa's connector to JELRT Viogt; Earlier used "VoigtJS" that is Jenya's Voigt function but is was WRONG
     +        SigPhI, FWevNatu, StJen, pqnUp

      if(La .ne. 2) PAUSE 'La =/= 2 in subr EmiAbso'

      do lw= 1, linM4      ! line# runs thru all XE along hvC(iw); Weak lines and out-of-hv-interval lines are already excluded
        EmiLFul(lw)= zero
         AbsoAm(lw)= zero
         AlfaBB(lw)= zero
        FWevLor(lw)= zero
        if(hvC(lw) .lt. hvSmo)     STOP '  Line with hvC < hvSmo'
        if(hvC(lw) .gt. hvMax*0.9) STOP '  Line with hvC > hvMax'
        kU = nUp(lw)
        kL = nLo(lw)
        nX = nX3(lw)	                                   ! nX3(iw) is XE# for this line
        if(kU.LT.Nnu(nX) .AND. BE(kU).LT. 1.d-3) goto 1  ! skip line from non-AI dead EL
        if(kL.LT.Nnu(nX) .AND. BE(kL).LT. 1.d-3) goto 1  ! skip line onto non-AI dead EL

        EmiLFul(lw)= BolJ*hvC(lw)*A(kU,kL,nX)*           ! In-Full-Shape Emissivity, W/cc/sr
     +                    POPt(kU,nX,La)*Den(nX)/FoPi
        if(POPt(kL,nX,La).gt.1.d-15) AlfaBB(lw)=
     +                               (POPt(kU,nX,La)/g0(kU,nX))/
     +                               (POPt(kL,nX,La)/g0(kL,nX))
        AbsoAm(lw)= 1.098d-16* flu(kL,kU,nX) *POPt(kL,nX,La)*Den(nX) ! Absorptivity (16) except *P(hv); i.e. [eV/cm], see (18)
     +                       * (one-AlfaBB(lw))
  1     continue

        FWevNatu= h* Wout(kU,nX,La)/ToPi  ! eV; Smallest possible Lorentzian FWHM.

        StJen = 0.    ! Remains for all spectral lines except resonance K-shell lines.
c  Approximate expression for Lyman lines and similar lines of He-like ions, Here for Aluminum and Mg, based on "Interactive Formulary"
        if(hvC(lw).gt. PI(HSS(nX),nX)/2.) then  ! [eV], "PI(H-like)/2" separates resonance K-shell lines from softer lines
          pqnUp= nprin(kU,nX)                   ! The lines are emitted in transitions of OuterMost e.

          if(pqnUp.eq.2 .and. nX.eq.1) StJen= 0.144*          ! [eV] AL Ly-A and He-A using AL LyA scaling in Formulary of Nov 1, 2022
     +              (Dene/1.3d22)**0.405 * (Te/1.d3)**0.463   ! assuming pure AL, Ti= 2Te, Z=13 for Te= (1- 1.4) keV, ni = (1-4)d21 i/cc

          if(pqnUp.eq.2 .and. nX.eq.2) StJen= 0.199*          ! [eV] MgLyA and MgHeA based on scaling of Mg Ly-A in Formulary of Nov 4, 2022 assuming
     +              (Dene/1.3d22)**0.398 * (Te/1.d3)**0.454   ! 5% Mg in Al plasma of Z=13, Ti= 2Te, Te= (1-1.4) keV, ni = (1-3)d21 i/cc

          if(pqnUp.eq.3 .and. nX.eq.1) StJen= 7.0 *           ! Al Ly-b (at hv= 2.0483 keV) is out of TX spectrum but adds to radiative loss.
     +                   (Dene/3.d22)**0.6 *(Te/1.d3)**0.2    ! Formulary; Nov 4, 2022, pure AL, Ti=2Te, Z=13, Te= (1- 1.4) keV, ni= (1-3)d21 i/cc

          if(pqnUp.eq.3 .and. nX.eq.2) StJen= 5.08 *          ! Mg Ly-b (hv= 1.7447 keV in by-FAC Database used here). Formulary of Nov 4, 2022
     +              (Dene/1.3d22)**0.566 *(Te/1.d3)**0.324    ! assuming 5% Mg in Al plasma of Z=13, Ti= 2Te, Te= (1-1.4) keV, ni= (1-3)d21 i/cc

          if(pqnUp.eq.4) StJen= 9.3 *(AtMass(1)/AtMass(nX))*  ! The Formulary gave 9.28 for AL HeG
     +                   (Dene/1.d22)**0.6 *(Te/1.d3)**0.2
        endif

        FWevGau= hvC(lw)* FWkVcGAU(nX)     ! [eV] Gauss FWHM of line #'lw' in plasma with TiD(La)
        FWevLor(lw)= max(FWevNatu, StJen)  ! [eV] Lorentz FWHM; Note: "StJen" =/= 0 only for resonance K-shell lines and their satellites.
c                       "FWevNatu" =0 for cut (above-PIR) levels because their POP and "Wout(kU,nX,La)"==0
        HaLor= FWevLor(lw)/2.
        FWvoi(lw)= HaLor + sqrt(HaLor**2 +FWevGau**2)  ! from JQSRT on Voigt subr

        if(ng .eq. gpEq(PrFr) .and. POPt(kU,nX,La).gt. 1.d-8	          ! "ng" is serial number of "tf" point on t axis (thus, the number of "ti-tf" interval)
     +                        .and. hvC(lw) .GT. hvSmo)                 ! "gpEq(j)" is the NUMBER of t-point at which tf = tres(j)
     +     write(46,'(f9.3, 2i4, f11.4, i6,a1,a4,a4, i6,a1,a4,a4,9e9.3) ! (46 is "LineWidth.dat"
     +              ') hvC(lw), nX, kiSS(kU,nX), eVA/hvC(lw),
     +           kU, '=', QSname1(kU,nX), QSname2(kU,nX),
     +           kL, '=', QSname1(kL,nX), QSname2(kL,nX), FWkVcGAU(nX), ! Gau FW(TiD)/hvC; "FWkVcGAU(nX)" is determ via TiD(La) and Imas(nX) ,
     +           StJen/hvC(lw), FWevNatu/hvC(lw), FWevLor(lw)/hvC(lw),
     +              FWvoi(lw)/hvC(lw), Wout(kU,nX,La), Wout(kL,nX,La),  ! 1/s
     +                            POPt(kL,nX,La), POPt(kU,nX,La)
      enddo ! lw

c  BB  EMISSION and ABSORPTION due to spectral lines of ALL XE;  Weak lines are excluded from LineList
c                            Lines from cut ELs and onto cut ELs came with EmiLFul(lw)=0, AbsoAm(lw)=0
       emisBB= zero
      absorBB= zero
      do lw= 1, linM4  !
        FWevGau = hvC(lw)* FWkVcGAU(nX3(lw))  ! Gaussian FWHM [eV] due to TiD
        Wing    = hvC(lw)/2.                  ! Line Wing Length [eV]
        ArPBB(lw) = zero      ! Area under line shape pBB(v), was =1 until wings cut
        do iv= 1, nvM
           dev  = abs(hvV(iv)-hvC(lw))
           pBB(iv) = zero                     ! this lw
           if(hvV(iv).GT. hvC(lw)-Wing .and.
     +        hvV(iv).LT. hvC(lw)+Wing      ) then
              pBB(iv) = Voigt2(FWevLor(lw), FWevGau, hvC(lw), dev)  ! JELRT (Sawa) Voigt. It is slower (5x ?? than Jenya's VoigtJS) but correct to 1%
           endif
           dhv = hvV(iv)-hvV(iv-1)
           if(iv.GT.1) ArPBB(lw)= ArPBB(lw) +     ! Area under shape < 1 because line wings cut thus part of photons lost.
     +       	         dhv*(pBB(iv)+pBB(iv-1))/2. ! To return them, cut-wings shape will be re-norm to Integ[pBB(v)dv] = 1
        enddo   ! iv

        if(ArPBB(lw) .LT. 0.0) then !Pavel set 0.0 instead of 0.1
           write(*,'(/a36, i6, f10.3, 9e10.2)')
     +       'lw, hvC, FWevGau, ArPBB(lw) =',
     +        lw, hvC(lw), FWevGau, ArPBB(lw)
	     STOP '  ArPBB(lw) < 0.01)'
        endif

        do iv= 1, nvM
           emisBB(iv)= emisBB(iv)+ EmiLFul(lw) *pBB(iv) ! BB emissivity [W/cc/sr/eV] DUE TO ALL lines of this XE, at hveV
     +                                       /ArPBB(lw) ! return photons from cut wings
          absorBB(iv)= absorBB(iv)+ AbsoAm(lw) *pBB(iv) ! [eV/cm]*[1/eV] = 1/cm  (16) DUE TO ALL LINES
     +                                       /ArPBB(lw) ! return photons from cut wings
        enddo
      enddo     ! lw

c  FF  EMISSION and ABSORPTION due to scattering of ALL free electrons on ions of ALL XE.
      do iv= 1, nvM
         hveV = hvV(iv)
         DiZ2= DenI*ZC2                                           ! all XE, see definition of ZC2
         emisFF(iv) = 1.21d-33*DiZ2*Dene*exp(-hveV/Te)/sqrt(Te)   ! [W/cc/sr/eV], (61); MAXWELLIAN EED
         absorFF(iv)= 2.4006d-37* Dene*DiZ2 *(1.-exp(-hveV/Te))/  ! (74), including induced-emi correction in Maxw case.
     +	                                    sqrt(Te)/hveV**3    ! Notice 1% correction in 2.4006 to fit 5040.36 W in Planck.
      enddo

c  FB EMISSION due to RR on ions of both       Notations: Recomb (SS=j+1,k) + e  --> (j, kf) + hv
      if(ti.LT.strt+ 2*tstep) goto 90        ! Skip FBBF on 1st t-step because BE not computed
      do iv= 1, nvM
        emisFB(iv)= zero
        hveV = hvV(iv)
        do nX= 1, nXE
          do k = 1, NST(nX)
            do kf= 1, NST(nX)
               if(bra(kf,k,nX).LT. 0.5)  goto 65  ! no Ionz/Recomb link
               j = KiSS(kf,nX)
               if(k .LT.Nnu(nX) .AND. BE(k ).LT. 1.d-3) goto 65  ! dead non-AI EL; Note: AIELs are treated as "always alive"
               if(kf.LT.Nnu(nX) .AND. BE(kf).LT. 1.d-3) goto 65  ! deak non-AI EL        although given BE= -13 in Intro

               BEk= Eth(kf,k,nX) - DPI(j,nX)   ! min energy for ionization (j,kf) --> (j+1,k)
               if(kf.GT.Nnu(nX) .AND.          ! AI EL
     +            BEk .LT. zero)   BEk= 0.01   ! because of big DPI, this kf-->k inz of kf=AI
c                                              does not require E, but I give formal BEk= 0.01 eV to avoid /0.
c                                                  Note: "BEk < 0" never happens to non-AI kf because big DPI cuts it.
               if(hveV .LT. BEk +1.d-7)  goto 65   ! such small'hv' cannot be emitted in this k\kf recomb; 1.d-7 excludes FrE=0
               FrE= hveV - BEk                     ! energy of free electron before capture:  hv= FrE +BEk
               Vel= 5.930968d7*sqrt(FrE)           ! Velocity of free electr = sq(2k*E/m); cm/s
               hvJ= BolJ*hveV

               ReverseKKF = 2.                          ! to use function "SigRR(FrE)" reverse k and kf because "SigRR" uses notations of "SigPhi"
               contrib= Dene*hvJ*Den(nX)*POPt(k,nX,La)* ! one k-->kf channel to (37) sum
     +                  Vel*EED(FrE)*SigRR(FrE)/FoPi
               emisFB(iv)= emisFB(iv) + contrib         ! Sum (37), W/cc/sr/eV
 65           continue
            enddo      ! kf
          enddo        ! k
        enddo          ! nX
      enddo            ! iv

c  BF ABSORPTION corrected for induced inverse process. Expressions (50) and (49) in BlackFold.
c                              Notations: (j,k) + hv -->(j+1,kf) + e , like in function "SigPhi" to be used
      do iv= 1, nvM
        absorBF(iv)= zero
        hveV = hvV(iv)
        do nX= 1, nXE
          do k = 1, NST(nX)
            do kf= 1, NST(nX)
               if(bra(k,kf,nX).LT. 0.5)                 goto 83  ! no Ionz/Recomb link
               if(k .LT.Nnu(nX) .AND. BE(k ).LT. 1.d-3) goto 83  ! dead non-AI EL; Note: AIELs are treated as "always alive"
               if(kf.LT.Nnu(nX) .AND. BE(kf).LT. 1.d-3) goto 83  ! deak non-AI EL        although given BE= -13 in Intro

               j = KiSS(k ,nX)
               BEk= Eth(k,kf,nX) - DPI(j,nX)  ! min energy for (j,kf) to (j+1,k) ionization is softest-photon hv emitted in RR; this photon is due to free el with E=0.
               if(k.GT.Nnu(nX) .AND.          ! AIEL
     +            BEk .LT. zero)    BEk= 0.1  ! because of big DPI, k-->kf inz of k=AI
c                                             does not require E, but I give formal BEk= 0.1 eV to avoid 0.
c                                                  Note: "BEk < 0" never happens for non-AI k because big DPI cuts it.
               if(hveV .LT. BEk +1.d-7)  goto 83   ! such small'hv' cannot photo-ionize k to kf; 1.d-7 excludes FrE=0
               FrE= hveV - BEk                     ! energy of free electron before capture:  hv= FrE +BEk
               Vel= 5.930968d7*sqrt(FrE)           ! Velocity of free electr = sq(2k*E/m); cm/s

               Ww= 1.46796094d-22*Dene*EED(FrE)/sqrt(FrE)  ! We write now "1.468" instead of "2.936../two" because already proven that our guess on the error in (49) is correct, see comment in true BlackFold
               popgrat= POPt(kf,nX,La)*g0(k ,nX)/
     +                  POPt(k, nX,La)/g0(kf,nX)
               alfaFB= Ww*popgrat
               contrib= Den(nX)*POPt(k,nX,La)*SigPhI(hveV)*(one-alfaFB)  ! (49)
               absorBF(iv)= absorBF(iv)+ contrib                         ! (50)

 83            continue
            enddo       ! kf -loop
          enddo         ! k  -loop
        enddo	          ! nX-loop
      enddo             ! iv-loop
 90   continue

      do iv= 1, nvM
         emTot(La,iv) =  emisBB(iv)+  emisFF(iv)+  emisFB(iv)  ! W/cc/sr/eV;
         abTot(La,iv) = absorBB(iv)+ absorFF(iv)+ absorBF(iv)  ! 1/cm;
      enddo   ! iv-loop

      if(ng .eq. gpEq(PrFr)) then    ! "ng" in  the NUMBER of "tf"-point in t axis
         open(81, file= 'EmiAbsoDetails.dat')
         do iv= 1, nvM
            if(iv.eq.1) write(81,'(a102)')
     +           'hvEV     iv     emisBB     emisFF     emisFB     emTot
     +     absorBB    absorFF    absorBF     abTot'

            write(81, '(f9.3, i7, 8e11.4)')  hvV(iv),  iv,
     +          emisBB(iv),  emisFF(iv),  emisFB(iv), emTot(2,iv),
     +         absorBB(iv), absorFF(iv), absorBF(iv), abTot(2,iv)
         enddo
         close(81)
      endif

      Return
      END     ! of 'EmiAbso' subr


      SUBROUTINE SCENARIO(ngw) ! gives R, Te, TiD, Den(nXE), %Mg, bp, bc, bw.
      use mo1                  ! spheric plasma zone of interest is mentioned as BS (bright source), La=2, "La" is the  namber of a zone               	.
      implicit none
      integer ngw   ! Serial number of "tf"-point in t axis; tf = tgp(ngw);
*                           gpEq(j) is # of t axis point at which tf=tres(j); j= 1,2,3...6
      real(8) dev_t, HWL  ! abs(t-peak time of Te(t))

      if(La.ne.2) STOP 'Came in scenario with La =/= 2'

        if(ngw .LE. gpEq(1)) then	! i.e. up to t of 1st "print frame"
           frac = (tgp(ngw)-tgp(1))/(tgp(gpEq(1)) -tgp(1))
           R2   = ( Dstrt + frac* ( Dfr(1) - Dstrt ))/2.     ! [cm]  Outer radius of BS, (La=2)
           Te   =  TeStrt + frac* (TeFr(1) - TeStrt)         ! [eV]  electron temperature in La=2
           TiD  = (TDstrt + frac* (TDfr(1) - TDstrt))*1000.  ! [eV]  Doppler temperature in BS
           DenI =   nStrt + frac* ( nFr(1) - nStrt )         ! ion number density [cm^-3]; AL+MG
           paMG = (peMGst + frac* (peMG(1) - peMGst))/100.   ! part of Mg ions in Mg+Al mixture, i.e. in "DenI"
           goto 5
        endif

        do i= 1, mSpe-1
          if(ngw.GT.gpEq(i) .and. ngw.LE.gpEq(i+1)) then
            frac = (tgp(ngw) - tgp(gpEq(i))) /
     +             (tgp(gpEq(i+1)) -tgp(gpEq(i)))
            R2   = ( Dfr(i) + frac* ( Dfr(i+1) - Dfr(i)))/2.    ! [cm].
            Te   =  TeFr(i) + frac* (TeFr(i+1) - TeFr(i))       ! [eV]
            TiD  = (TDfr(i) + frac* (TDfr(i+1) - TDfr(i)))*1000 ! [keV] to [eV]
            DenI =   nFr(i) + frac* ( nFr(i+1) -  nFr(i))       ! [i/cc]  AL+MG
            paMG = (peMG(i) + frac* (peMG(i+1) - peMG(i)))/100. ! from % to "part of DenI"
            goto 5
          endif
        enddo

        if( ngw.GT.gpEq(mSpe)) then   ! after last PrintFrame time
            R2   =  Dfr(mSpe)/2.      ! [cm]
            Te   = TeFr(mSpe)         ! [eV]
		  TiD  = TDfr(mSpe)*1000.   ! [keV] to [eV]
            DenI =  nFr(mSpe)         ! [i/cc]  AL+MG
            paMG = peMG(mSpe)/100.    ! from % to "part of DenI"
            goto 5
        endif
  5     continue

        if(KeySmooth .GT. 1) then  ! Replace broken-line t-dependence of Te(t)s with smooth Lorentzian type t-dependence
c                                  to avoid discontinuities in dTe/dt which cause jumps in Ti(t) via dTe/dt term in eq. (3).
c                                               "ngw" is the point# on t axis;
c                                               "gpEq(j)" is t axis point# at which t = tres(j)
                                     HWL = HWL1    ! [s] HWHM of before-peak Te(t)
           if(ngw.GT.gpEq(tresPeak)) HWL = HWL2    ! [s] HWHM of  after-peak Te(t)
           dev_t = tgp(ngw) - tgp(gpEq(tresPeak))  ! [s] delta-t from the time of Te-peak.
           Te = PeakTe/(1.+ (dev_t/HWL)**2)        ! [eV]
        endif

        TeBS = Te
        Volu(2)= 1.3333*pin* R2**3    ! [cm3] Volume of spherical BS

        Den(1)= DenI*(1.-paMG)  ! (ions ofAL)/cc; Here "1" means nXE # 1 that is AL,
        Den(2)= DenI*paMG       ! for "paMG= 0.0552" see  "****" 3 lines sbove

c  Note: 5% of mass in the alloy results in 5.844% in ion number because
c            mass of Mg in the mixture "Mmg" = 0.05 (Mal+Mmg) can be written as
c                                  24.30*nMG = 0.05 [24.30*nMG +26.98*nAL)
c           thus 26.98*nAL = 20*24.30*nMG -24.3*nMG = 19.*24.30*nMG ,
c           thus nMG = nAL * 26.98/(19*24.30) = 0.05844 * nAL.
c                nAL+nMG = DenI thus [nAL + 0.05844*nAL = DenI]
c           thus nAL= DanI/1.05844 = DenI * 0.9448;
c           thus nMG= DenI - nAL = DenI * 0.0552

        MeanM = Imas(1)*(1.-paMG) + Imas(2)*paMG   ! [g]. not A.U.

      distII= one/DenI**third  ! cm; mean ion-ion distance
      ZC1 = zero               ! mean ion charge   in the layer: average over XE and SS (including atoms)
      ZC2 = zero               ! mean ion charge^2 in the layer: average over XE and SS (including atoms)
      Dene= zero               ! e/cc in the Layer
      do n7= 1, nXE
        ZC(n7)= zero
      do  k= 1, NST(n7)
        ZC(n7)= ZC(n7)+                POPt(k,n7,La)*(kiSS(k,n7)-1)      ! Note: these POPs are from previous t-step
        ZC1   = ZC1   + (Den(n7)/DenI)*POPt(k,n7,La)*(kiSS(k,n7)-1)
        ZC2   = ZC2   + (Den(n7)/DenI)*POPt(k,n7,La)*(kiSS(k,n7)-1)**2
        Dene  = Dene  +  Den(n7)      *POPt(k,n7,La)*(kiSS(k,n7)-1)
      enddo
      enddo
      Zbar(La)= ZC1
      bp= zero      ! beam part of the EED
      bc= 12.d3     ! [eV] central energy of e-beam,
Check: max energy of the beam, (bc+bw/2) must be within the hv interval, i.e. < hvMax.
      bw= bc/10.d0  ! e-beam width , eV
      Return
      END

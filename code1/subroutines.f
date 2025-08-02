

      SUBROUTINE redPI()  ! gives Ionization Potentials for {Te(ti-tf),ni(ti-tf)} of MAIN.
	use mo1             ! two options: "table values (no lowering)", "ion-sphere", depending on "KeReDu" in Flag.inp
	implicit none
	real(8) SpheR, DPIsph(HSSm+1)

      if(ng .eq. gpEq(PrFr)) then   ! "ng" in  the NUMBER of "tf"-point in t axis
         write(41,'(/a7, f5.3, a21)') 'tf=', tf*1.d9,
     +                                         'ns is the PrFr time:'
         write(41,'(/a33)') ' SpS   tablePI    IonSphDPI    PIR'
      endif

      SpheR=(three/FoPi/DenI)**third   ! Ion Sphere radius from 1= 4piR^3*Ni/3; Gr3 (7.36)

      do jSS = FSS(nX), HSS(nX)        ! all ionizible specroscopic symbols
	  DPIsph(jSS) = 13.606*dble(jSS)* 1.5*0.5292d-8/SpheR  ! [eV], Griem3 (7.43)
  	  if(KeRedu .eq.-1) DPI(jss,nX)= zero          ! no lowering
  	  if(KeRedu .eq. 0) DPI(jss,nX)= DPIsph(jss)   ! ion sphere

        PIR(jSS,nX)= PI(jSS,nX) -DPI(jSS,nX)
        if(PIR(jSS,nX) .lt. PI(jSS,nX)/5.) stop '  PIR < PI/5.'

        if(ng .eq. gpEq(PrFr)) write(41,'(i3,3f11.3)') jSS,
     +                            PI(jSS,nX),  DPIsph(jSS),  PIR(jSS,nX)
	enddo
      Return
      END   ! of 'redPI' subr



      SUBROUTINE BEvPr()  ! gives Binding Energy of outermost electron (BE)
	use mo1
	implicit none
      do k= 1, NST(nX)
        BE(k) = -13.d0  ! '-13' remains for AI ELs
      enddo

      do j= FSS(nX), HSS(nX)
         do k= nuGS(j,nX), nuGS(j+1,nX)-1   ! non-AI ELs of j
            BE(k)= PIR(j,nX) - E(k,nX)      ! above-PIR ELs get BE < 0
	   enddo
      enddo
      BE(Nnu(nX))= 2.d-3  ! [eV] Nucleus is given Artificial (wrong) small BE > 0 to  avoid its de-POP by "if(BE < 1.d-3)"
      Return
      END     ! of 'BEvPr' subr


      SUBROUTINE POPdot(tSC, wPOP, dPOPpdt)
	use mo1
	implicit none
	real(8) tSC, wPOP(NSTm), dPOPpdt(NSTm), sum, xx
      do k = 1, NST(nX)
        sum = zero
        do k1 = 1, NST(nX)              ! For k1=k the PM(k,k)= -Wout(k,nX,La)
          sum = sum+ wPOP(k1)*PM(k1,k)  ! for RateEq dPOP(k)/dt= sum(k)
        enddo
        dPOPpdt(k)= sum/Scale           ! for slow-time d02
      enddo
      xx= tSC*zero   ! to avoid "tSC not used" warning
      Return
      END




! T2: one time step of all the physics on a real database (Kr or Ge), about a minute.
! Run in a folder with the input files of the case. The program reads the
! database as the code does (OpenFiles, Intro, LineList, hvPoints), sets the
! populations of element X to a fixed synthetic distribution over three ion
! stages (different in each zone), sets ti so that the bound-bound part of
! EmiAbso is active, and calls every routine of one time step once:
! SCENARIO, LevStW, redPI, BEvPr, EmiAbso, EffSpIn, PowYie, AtKins (Ws, PMg0,
! D02EAF). Each result array is written as a bit hash (+ sum, max) and
! run_tests.py compares the file with golden/step_<case>.txt.
program test_step
  use mo1co2feb
  use testio
  implicit none
  character(256) :: outf
  character(32)  :: zn
  integer :: tLa, tk, s0, tdm
  real(8) :: tsum

  outf = 'step_out.txt'
  if (command_argument_count() >= 1) call get_command_argument(1, outf)

  call AllocArrays()
  call OpenFiles()
  call Intro()
  call LineList()
  call hvPoints()
  call topen(trim(outf))

  ! ---- database as read by Intro / LineList / hvPoints
  call reci('nQSm', nQSm);  call reci('HSSm', HSSm);  call reci('MNLe', MNLe)
  call reci('NST1', NST(1)); call reci('nvM', nvM);  call reci('linM4', linM4)
  call reca('hvV', hvV, nvM)
  call reca('hvC', hvC, linM4)
  call recia('nUp', nUp, linM4);  call recia('nLo', nLo, linM4)
  call reca('E', E, size(E));     call reca('g0', g0, size(g0))
  call reca('A', A, size(A));     call reca('flu', flu, size(flu))
  call reca('WAiz', WAiz, size(WAiz));  call reca('bra', bra, size(bra))
  call reca('Eth', Eth, size(Eth));     call reca('EAI', EAI, size(EAI))
  call reca('Ax', Ax, size(Ax));  call reca('Fx', Fx, size(Fx))
  call recia('MthdEX', MthdEX, size(MthdEX))
  call reca('Aix', Aix, size(Aix));  call reca('Hix', Hix, size(Hix))
  call reca('PI', PI, size(PI));     call recia('kiSS', kiSS, size(kiSS))
  call reca('Resp', Resp, size(Resp))
  call reca('POPf0', POPf, size(POPf))

  ! ---- synthetic populations of X: three ion stages starting at FSS+La-1,
  !      Boltzmann-like over the non-AI levels, normalized to 1
  do tLa = 1, LaMx
    s0 = FSS(1) + tLa - 1
    POPf(:,1,tLa) = 0.d0
    do tk = 1, Nnu(1) - 1
      tdm = kiSS(tk,1) - s0
      if (tdm >= 0 .and. tdm <= 2) &
        POPf(tk,1,tLa) = g0(tk,1) * exp(-E(tk,1)/(300.d0*tLa)) * (0.5d0**tdm)
    end do
    tsum = sum(POPf(1:NST(1),1,tLa))
    POPf(:,1,tLa) = POPf(:,1,tLa) / tsum
  end do

  ! ---- one time step, as in the main program, with BB emission active
  ti = strt + 2*tstep
  tf = ti + tstep
  call rec('ti', ti)
  do La = 1, LaMx
    write(zn,'(a,i1,a)') 'z', La, '_'
    CALL SCENARIO()
    call rec(trim(zn)//'Te', Te(La));   call rec(trim(zn)//'Dene', Dene(La))
    call rec(trim(zn)//'ZC1', ZC1(La)); call rec(trim(zn)//'SigMax', SigMax(La))
    do nX = 1, 1
      CALL LevStW()
      CALL redPI()
      CALL BEvPr()
      FWkVcGAU(La,nX)= 7.7155e-5*sqrt(TD(La)/AtMass(nX))   ! same expression as in the main program
    enddo
    CALL EmiAbso()
    call reca(trim(zn)//'POPi', POPi(:,1,La), NST(1))
    call reca(trim(zn)//'BE', BE(:,1,La), NST(1))
    call reca(trim(zn)//'LvJW', LvJW(La,1,:), NST(1))
    call reca(trim(zn)//'DPI', DPI(:,1,La), HSSm)
    call reca(trim(zn)//'FWevLor', FWevLor(:,La), linM4)
    call reca(trim(zn)//'EmTot', EmTot(La,1:nvM), nvM)
    call reca(trim(zn)//'AbTot', AbTot(La,1:nvM), nvM)
    call reca(trim(zn)//'emisBB', emisBB, nvM)
    call reca(trim(zn)//'emisFB', emisFB, nvM)
    call reca(trim(zn)//'absoBF', absoBF, nvM)
  end do

  CALL EffSpIn()
  call reca('SpInEf', SpInEf, size(SpInEf))
  CALL PowYie()
  call reca('SpPow', SpPow, nvM)
  call reca('FrYie', FrYie, size(FrYie))
  call reca('SpIn', SpIn, size(SpIn))

  do La = 1, LaMx
    write(zn,'(a,i1,a)') 'z', La, '_'
    do nX = 1, 1
      if (Dene(La) .gt. 1.e15) then
        CALL AtKins()
        call reca(trim(zn)//'WI', WI, size(WI));     call reca(trim(zn)//'WTB', WTB, size(WTB))
        call reca(trim(zn)//'WPhI', WPhI, size(WPhI)); call reca(trim(zn)//'WRR', WRR, size(WRR))
        call reca(trim(zn)//'WiRR', WiRR, size(WiRR)); call reca(trim(zn)//'WEX', WEX, size(WEX))
        call reca(trim(zn)//'WDX', WDX, size(WDX));   call reca(trim(zn)//'WDC', WDC, size(WDC))
        call reca(trim(zn)//'WInd', WInd, size(WInd)); call reca(trim(zn)//'Wab', Wab, size(Wab))
        call reca(trim(zn)//'PM', PM, size(PM));     call reca(trim(zn)//'Wout', Wout, size(Wout))
        call reca(trim(zn)//'POPf', POPf(:,1,La), NST(1))
        call reca(trim(zn)//'POPZ', POPZ(:,1,La), HSSm+1)
      else
        call reci(trim(zn)//'AtKins_skipped', 1)
      endif
    enddo
  end do
  call tclose()
end program test_step

! Independent GNU Fortran replacements for two legacy NAG entry points.
! D01AHF: adaptive 15-point Gauss-Kronrod quadrature.
! D02EAF: matrix-exponential propagation for a frozen linear rate matrix.
! The latter is specific to the Code1/Code2 POPdot (dPOP/dt = PM^T POP / Scale) and is not a general ODE solver.
module co2numerics
 implicit none
contains
 recursive subroutine quad(f,a,b,eps,depth,val,err,evals)
  interface
   function f(x) result(y)
    real(8),intent(in)::x
    real(8)::y
   end function
  end interface
  real(8),intent(in)::a,b,eps
  integer,intent(in)::depth
  real(8),intent(out)::val,err
  integer,intent(inout)::evals
  real(8),parameter::x(8)=[0.9914553711208126d0,0.9491079123427585d0, &
   0.8648644233597691d0,0.7415311855993945d0,0.5860872354676911d0, &
   0.4058451513773972d0,0.2077849550078985d0,0.d0]
  real(8),parameter::wk(8)=[0.0229353220105292d0,0.0630920926299786d0, &
   0.1047900103222502d0,0.1406532597155259d0,0.1690047266392679d0, &
   0.1903505780647854d0,0.2044329400752989d0,0.2094821410847278d0]
  real(8),parameter::wg(4)=[0.1294849661688697d0,0.2797053914892766d0, &
   0.3818300505051189d0,0.4179591836734694d0]
  real(8)::mid,half,fg,fk,v1,v2,ff,vl,vr,el,er
  integer::i
  mid=(a+b)/2; half=(b-a)/2
  fk=wk(8)*f(mid); fg=wg(4)*f(mid); evals=evals+1
  do i=1,7
   v1=f(mid-half*x(i)); v2=f(mid+half*x(i)); evals=evals+2
   ff=v1+v2; fk=fk+wk(i)*ff
   if(i==2)fg=fg+wg(1)*ff
   if(i==4)fg=fg+wg(2)*ff
   if(i==6)fg=fg+wg(3)*ff
  end do
  val=fk*half; err=abs(fk-fg)*abs(half)
  if(err<=eps*max(1.d-50,abs(val)) .or. depth<=0) return
  call quad(f,a,mid,eps,depth-1,vl,el,evals)
  call quad(f,mid,b,eps,depth-1,vr,er,evals)
  val=vl+vr;err=el+er
 end subroutine
end module

real(8) function D01AHF(a,b,eps,npts,rer,f,nlim,ifail) result(ans)
 use co2numerics
 implicit none
 real(8),intent(in)::a,b,eps
 integer,intent(inout)::npts,nlim,ifail
 real(8),intent(out)::rer
 interface
  function f(x) result(y)
   real(8),intent(in)::x
   real(8)::y
  end function
 end interface
 integer::ne
 if(b<=a)then
  ans=0;rer=0;npts=0; ifail=0;return
 endif
 ne=0
 call quad(f,a,b,max(eps,1.d-12),15,ans,rer,ne)
 npts=ne
 ifail=0
end function

subroutine D02EAF(t0,t1,n,y,tol,deriv,work,lwork,ifail)
 use mo1co2feb, only: PM, nX, NST, Scale
 implicit none
 real(8),intent(in)::t0,t1,tol
 integer,intent(in)::n,lwork
 integer,intent(inout)::ifail
 real(8),intent(inout)::y(n),work(*)
 external deriv
 real(8),allocatable::a(:,:),term(:,:),tmp(:,:),e(:,:),v(:)
 real(8)::dt,norm1,fac
 integer::i,j,k,s
 allocate(a(n,n),term(n,n),tmp(n,n),e(n,n),v(n))
 dt=(t1-t0)/Scale
 ! PM(source,destination) is the transition-rate generator.
 a=transpose(PM(1:n,1:n))*dt
 norm1=maxval(sum(abs(a),dim=1))
 s=max(0,ceiling(log(max(norm1,1.d-300)/0.25d0)/log(2.d0)))
 if(s>100)then
  ifail=1;deallocate(a,term,tmp,e,v);return
 endif
 a=a/2.d0**s
 e=0;term=0
 do i=1,n
  e(i,i)=1;term(i,i)=1
 end do
 do k=1,30
  tmp=matmul(term,a)/dble(k)
  term=tmp;e=e+term
  if(maxval(abs(term))<1.d-15)exit
 end do
 do k=1,s
  tmp=matmul(e,e);e=tmp
 end do
 v=matmul(e,y)
 if(minval(v)<-1.d-10 .or. any(v/=v))then
  ifail=2
 else
  y=v
  ifail=0
 endif
 deallocate(a,term,tmp,e,v)
end subroutine

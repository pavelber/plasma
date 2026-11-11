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

! Parallel matrix products with a fixed order of summation (Nov 2026): the result
! does not depend on the number of OpenMP threads.
module co2expm
 implicit none
contains
 subroutine pmm(a,b,c,n)                    ! c = a*b (n x n)
  ! Blocks: columns of c per thread (jb), rows (ib) and k (kb) so that a(i0:i1,k0:k1)
  ! stays in the cache. For each c(i,j) the sum over k goes in increasing k, whatever
  ! the block sizes and the number of threads.
  integer,intent(in)::n
  real(8),intent(in)::a(n,n),b(n,n)
  real(8),intent(out)::c(n,n)
  integer,parameter::jb=32,kb=128,ib=256
  integer::j0,j1,j,k0,k1,k,i0,i1
  !$omp parallel do schedule(dynamic,1) private(j0,j1,j,k0,k1,k,i0,i1)
  do j0=1,n,jb
   j1=min(n,j0+jb-1)
   do j=j0,j1
    c(:,j)=0.d0
   end do
   do k0=1,n,kb
    k1=min(n,k0+kb-1)
    do i0=1,n,ib
     i1=min(n,i0+ib-1)
     do j=j0,j1
      k=k0
      do while(k+3<=k1)                     ! 4 k at a time; the parentheses keep the
       c(i0:i1,j)=(((c(i0:i1,j)+a(i0:i1,k)*b(k,j))+a(i0:i1,k+1)*b(k+1,j)) &  ! order
                  +a(i0:i1,k+2)*b(k+2,j))+a(i0:i1,k+3)*b(k+3,j)            ! k, k+1, ...
       k=k+4
      end do
      do while(k<=k1)
       c(i0:i1,j)=c(i0:i1,j)+a(i0:i1,k)*b(k,j)
       k=k+1
      end do
     end do
    end do
   end do
  end do
  !$omp end parallel do
 end subroutine

 subroutine pmv(a,v,w,n)                    ! w = a*v
  integer,intent(in)::n
  real(8),intent(in)::a(n,n),v(n)
  real(8),intent(out)::w(n)
  integer,parameter::ib=128
  integer::i0,i1,k
  !$omp parallel do schedule(static) private(i0,i1,k)
  do i0=1,n,ib
   i1=min(n,i0+ib-1)
   w(i0:i1)=0.d0
   do k=1,n
    w(i0:i1)=w(i0:i1)+a(i0:i1,k)*v(k)
   end do
  end do
  !$omp end parallel do
 end subroutine
end module co2expm


subroutine D02EAF(t0,t1,n,y,tol,deriv,work,lwork,ifail)
 ! Matrix exponential for the frozen rate matrix PM:  y(t1) = exp(A) y(t0),
 ! A = transpose(PM)*(t1-t0)/Scale  (dPOP/dt = PM^T POP, see POPdot).
 ! Nov 2026, faster than D02EAF_TAYLOR (same method, other order of operations, so the
 ! last digits differ):
 !  - scaling: A/2^s with ||A/2^s||_1 <= 0.8;
 !  - exp(A/2^s) by the Taylor polynomial of degree 16 (truncation < 1e-16), evaluated
 !    with the Paterson-Stockmeyer scheme: 6 matrix products instead of ~13;
 !  - of the s squarings only s-10 are done on the matrix; the last 2^10 = 1024 factors
 !    are applied to the vector (1024 matrix-vector products ~ 0.6 matrix product);
 !  - matrix and matrix-vector products in parallel (OpenMP) with a fixed order of the
 !    sums, so the result does not depend on the number of threads.
 use mo1co2feb, only: PM, Scale
 use co2expm
 implicit none
 real(8),intent(in)::t0,t1,tol
 integer,intent(in)::n,lwork
 integer,intent(inout)::ifail
 real(8),intent(inout)::y(n),work(*)
 external deriv
 integer,parameter::nvec=10                 ! last 2^nvec factors applied to the vector
 real(8),parameter::theta=0.8d0
 real(8),allocatable::a(:,:),a2(:,:),a3(:,:),a4(:,:),x(:,:),tmp(:,:),v(:),w(:)
 real(8)::dt,norm1,c(0:16)
 integer::i,k,s,r,nrep
 allocate(a(n,n),a2(n,n),a3(n,n),a4(n,n),x(n,n),tmp(n,n),v(n),w(n))
 dt=(t1-t0)/Scale
 a=transpose(PM(1:n,1:n))*dt
 norm1=maxval(sum(abs(a),dim=1))
 s=max(0,ceiling(log(max(norm1,1.d-300)/theta)/log(2.d0)))
 if(s>100)then
  ifail=1;deallocate(a,a2,a3,a4,x,tmp,v,w);return
 endif
 a=a/2.d0**s
 c(0)=1.d0
 do k=1,16
  c(k)=c(k-1)/dble(k)
 end do
 call pmm(a,a,a2,n)                         ! A^2, A^3, A^4
 call pmm(a2,a,a3,n)
 call pmm(a2,a2,a4,n)
 ! Horner in A^4:  T = B0 + A4*(B1 + A4*(B2 + A4*(B3 + c16*A4))),
 ! Bj = c(4j) I + c(4j+1) A + c(4j+2) A^2 + c(4j+3) A^3
 call bj(3,x);  x=x+c(16)*a4
 call pmm(a4,x,tmp,n);  call bj(2,x);  x=x+tmp
 call pmm(a4,x,tmp,n);  call bj(1,x);  x=x+tmp
 call pmm(a4,x,tmp,n);  call bj(0,x);  x=x+tmp        ! x = exp(A/2^s)
 r=max(0,s-nvec)                            ! squarings of the matrix
 do k=1,r
  call pmm(x,x,tmp,n);  x=tmp
 end do
 nrep=2**(s-r)                              ! remaining factors on the vector
 v=y
 do k=1,nrep
  call pmv(x,v,w,n);  v=w
 end do
 if(minval(v)<-1.d-10 .or. any(v/=v))then
  ifail=2
 else
  y=v
  ifail=0
 endif
 deallocate(a,a2,a3,a4,x,tmp,v,w)
contains
 subroutine bj(jj,b)                        ! b = c(4j) I + c(4j+1) A + c(4j+2) A^2 + c(4j+3) A^3
  integer,intent(in)::jj
  real(8),intent(out)::b(n,n)
  b=c(4*jj+1)*a+c(4*jj+2)*a2+c(4*jj+3)*a3
  do i=1,n
   b(i,i)=b(i,i)+c(4*jj)
  end do
 end subroutine
end subroutine


subroutine D02EAF_TAYLOR(t0,t1,n,y,tol,deriv,work,lwork,ifail)
 ! The D02EAF replacement used until Nov 2026 (Taylor series, up to 30 terms, and
 ! s squarings with the gfortran MATMUL). Kept as the reference for the tests.
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

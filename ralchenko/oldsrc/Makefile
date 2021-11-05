FORTRAN=gfortran
CFLAGS=-O -mcmodel=large
LIBFLAGS=-lSystem /usr/local/gfortran/lib/libgfortran.a /usr/local/gfortran/lib/libquadmath.a /usr/local/gfortran/lib/gcc/x86_64-apple-darwin16/6.3.0/libgcc.a
DEPS=lmdif1.f

all: check check_bcfp check_rr ph_fac exc_fac

clean:
	rm *.o *.exe


check:
	$(FORTRAN) -c -o check.o $(CFLAGS) check.f


check_bcfp:
	$(FORTRAN) -c -o check_bcfp.o $(CFLAGS) check_bcfp.f

check_rr:
	$(FORTRAN) -c -o check_rr.o $(CFLAGS) check_rr.f

exc_fac:
	$(FORTRAN) -c -o exc_fac.o $(CFLAGS) fit_spline.f $(DEPS)

ph_fac:
	$(FORTRAN) -c -o ph_fac.o $(CFLAGS) ph_fac_new.f $(DEPS)

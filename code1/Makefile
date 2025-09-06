# Makefile for Windows using gfortran with Fortran 77 compatibility

.SUFFIXES: .for .f .o .exe

FORTRAN = gfortran
FFLAGS = -c -std=legacy -ffixed-form -fno-sign-zero -w

# Libraries in the same order as successful Makefile.dsl (converted to .a)
LIBS = libslatec.a libodepack.a libquadpack.a

# List of additional source files (including atkins.f, which uses DLSODE from ODEPACK)
ADDITIONAL_SOURCES = emiabso.f eff.f open_read.f linelist.f txscans.f rtespin.f scenario.f pmg0.f functions.f ws.f atkins.f subroutines.f powyie.f rkf45.f
ADDITIONAL_OBJS = emiabso.o eff.o open_read.o linelist.o txscans.o rtespin.o scenario.o pmg0.o functions.o ws.o atkins.o subroutines.o powyie.o rkf45.o

OBJS = mo1.o $(ADDITIONAL_OBJS)
EXE = Code1.exe

all: $(EXE)

# Link using the same approach as successful Makefile.dsl
# This ensures DLSODE from ODEPACK is found for atkins.f
$(EXE): Code1.for $(OBJS) $(LIBS)
	$(FORTRAN) -o $(EXE) Code1.for $(OBJS) $(LIBS)

# Compile main module
mo1: mo1.o

mo1.o: mo1.for
	$(FORTRAN) $(FFLAGS) -o $@ $<

# Pattern rule to compile .f files to .o files
%.o: %.f
	$(FORTRAN) $(FFLAGS) -o $@ $<

# Pattern rule to compile .for files to .o files (if needed)
%.o: %.for
	$(FORTRAN) $(FFLAGS) -o $@ $<

# Clean rule
clean:
	@if exist *.o del *.o
	@if exist *.obj del *.obj
	@if exist *.mod del *.mod
	@if exist $(EXE) del $(EXE)
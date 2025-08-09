# Makefile for Windows using nmake and fl32

.SUFFIXES: .for .obj .exe

FORTRAN = fl32
FFLAGS = -c

# Libraries in the same order as successful Makefile.dsl, plus naglib for additional symbols
LIBS = libslatec.lib libodepack.lib libquadpack.lib naglib.lib

# List of additional source files (including atkins.f, which uses DLSODE from ODEPACK)
ADDITIONAL_SOURCES = emiabso.f eff.f open_read.f linelist.f txscans.f rtespin.f scenario.f pmg0.f functions.f ws.f atkins.f subroutines.f powyie.f
ADDITIONAL_OBJS = emiabso.obj eff.obj open_read.obj linelist.obj txscans.obj rtespin.obj  scenario.obj pmg0.obj functions.obj ws.obj atkins.obj subroutines.obj powyie.obj

OBJS = mo1.obj $(ADDITIONAL_OBJS)
EXE = Code1.exe

all: $(EXE)

# Link using the same approach as successful Makefile.dsl
# This ensures DLSODE from ODEPACK is found for atkins.f
$(EXE): Code1.for $(OBJS) $(LIBS)
	$(FORTRAN) Code1.for $(OBJS) $(LIBS)

# Compile main module
mo1: mo1.obj

mo1.obj: mo1.for
	$(FORTRAN) $(FFLAGS) mo1.for

# Pattern rule to compile .f files to .obj files
%.obj: %.f
	$(FORTRAN) $(FFLAGS) $<

# Clean rule
clean:
	@if exist *.obj del *.obj
	@if exist $(EXE) del $(EXE)

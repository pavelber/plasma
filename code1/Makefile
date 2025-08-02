# Makefile for Windows using nmake and fl32

.SUFFIXES: .for .obj .exe

FORTRAN = fl32
FFLAGS = -c

# Additional source files to be compiled and linked with the main program
# To add more source files to the program:
# 1. Add the .f filename to the ADDITIONAL_SOURCES list below (space-separated)
# 2. Add the corresponding .obj filename to the ADDITIONAL_OBJS list
# 3. The object files will be automatically compiled and linked
ADDITIONAL_SOURCES = emiabso.f eff.f open_read.f linelist.f txscans.f rtespin.f scenario.f pmg0.f functions.f ws.f atkins.f subroutines.f powyie.f
ADDITIONAL_OBJS = emiabso.obj eff.obj open_read.obj linelist.obj txscans.obj rtespin.obj  scenario.obj pmg0.obj functions.obj ws.obj atkins.obj subroutines.obj powyie.obj

# Original object files
OBJS = mo1.obj $(ADDITIONAL_OBJS)
EXE = Code1.exe

all: $(EXE)

# Link all object files together to create the executable
$(EXE): Code1.for $(OBJS) naglib.lib
	$(FORTRAN) Code1.for $(OBJS) naglib.lib

mo1: mo1.obj

mo1.obj: mo1.for
	$(FORTRAN) $(FFLAGS) mo1.for

# Pattern rule to compile .f files to .obj files
# This automatically handles compilation of additional source files
%.obj: %.f
	$(FORTRAN) $(FFLAGS) $<

AlMg:
	cp mo1AlMg.for mo1.for
	$(MAKE)

KrMg:
	cp mo1KrMg.for mo1.for
	$(MAKE)

KrHe:
	cp mo1KrHe.for mo1.for
	$(MAKE)

clean:
	rm *.exe *.obj *.mod

# Makefile for Windows using nmake and fl32

.SUFFIXES: .for .obj .exe

FORTRAN = c:\tools\powerfortran\BIN\fl32
FFLAGS = -c

OBJS = mo1co2feb.obj
EXE = Co2feb26.exe

all: $(EXE)

$(EXE): Co2feb26.for $(OBJS) naglib.lib
	$(FORTRAN) Co2feb26.for $(OBJS) naglib.lib

mo1: mo1co2feb.obj

mo1co2feb.obj: mo1co2feb.for
	$(FORTRAN) $(FFLAGS) mo1co2feb.for

clean:
	rm *.exe *.obj *.mod

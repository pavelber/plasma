# Makefile for Windows using nmake and fl32

.SUFFIXES: .for .obj .exe

FORTRAN = c:\tools\powerfortran\BIN\fl32
FFLAGS = -c

OBJS = mo1co2nov.obj
EXE = Co2nov25.exe

all: $(EXE)

$(EXE): Co2nov25.for $(OBJS) naglib.lib
	$(FORTRAN) Co2nov25.for $(OBJS) naglib.lib

mo1: mo1co2nov.obj

mo1co2nov.obj: mo1co2nov.for
	$(FORTRAN) $(FFLAGS) mo1co2nov.for

clean:
	rm *.exe *.obj *.mod

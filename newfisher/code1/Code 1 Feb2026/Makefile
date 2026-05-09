# Makefile for Windows using nmake and fl32

.SUFFIXES: .for .obj .exe

FORTRAN = c:\tools\powerfortran\BIN\fl32
FFLAGS = -c

OBJS = mo1.obj
EXE = Code1_Feb2026.exe

all: $(EXE)

$(EXE): Code1_Feb2026.for $(OBJS) naglib.lib
	$(FORTRAN) Code1_Feb2026.for $(OBJS) naglib.lib

mo1: mo1.obj

mo1.obj: mo1.for
	$(FORTRAN) $(FFLAGS) mo1.for

clean:
	rm *.exe *.obj *.mod

# Makefile for Windows using nmake and fl32

.SUFFIXES: .for .obj .exe

FORTRAN = c:\tools\powerfortran\BIN\fl32
FFLAGS = -c

OBJS = mo1code2.obj
EXE = Code2.exe

all: $(EXE)

$(EXE): Code2.for $(OBJS) naglib.lib
	$(FORTRAN) Code2.for $(OBJS) naglib.lib

mo1: mo1code2.obj

mo1code2.obj: mo1code2.for
	$(FORTRAN) $(FFLAGS) mo1code2.for

clean:
	rm *.exe *.obj *.mod

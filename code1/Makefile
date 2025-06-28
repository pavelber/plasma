# Makefile for Windows using nmake and fl32

.SUFFIXES: .for .obj .exe

FORTRAN = fl32
FFLAGS = -c

OBJS = mo1.obj
EXE = Code1.exe

all: $(EXE)

$(EXE): Code1.for $(OBJS) naglib.lib
	$(FORTRAN) Code1.for $(OBJS) naglib.lib

mo1: mo1.obj

mo1.obj: mo1.for
	$(FORTRAN) $(FFLAGS) mo1.for

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

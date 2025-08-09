# Makefile for Netlib-only version

FORTRAN = fl32
FFLAGS = -c

PROGRAM = dlsode_test.exe
SOURCE = dlsode_test.for

# Try with minimal netlib libraries - remove quadlib.lib that might be causing conflicts
LIBS = libslatec.lib

all: $(PROGRAM)

$(PROGRAM): $(SOURCE)
	$(FORTRAN) $(SOURCE) $(LIBS)

clean:
	@if exist dlsode_test.obj del dlsode_test.obj
	@if exist $(PROGRAM) del $(PROGRAM)

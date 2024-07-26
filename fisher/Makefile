# Makefile for compiling Users_BS_1520_Dec2023.for, mo1.for, and naglib.lib

# Compiler and flags
FC = fl32.exe
FFLAGS = 

# Source files
SOURCES = Users_BS_1520_Dec2023.for mo1.for
LIBRARY = naglib.lib

# Output executable
OUTPUT = my_program.exe

# Targets
all: $(OUTPUT)

$(OUTPUT): $(SOURCES) $(LIBRARY)
	$(FC) $(FFLAGS) $(SOURCES) $(LIBRARY)

clean:
	del $(OUTPUT)

.PHONY: all clean

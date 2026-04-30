# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Co2feb26 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Co2feb26 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Co2feb26 - Win32 Release" && "$(CFG)" !=\
 "Co2feb26 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "Co2feb26.mak" CFG="Co2feb26 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Co2feb26 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "Co2feb26 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
RSC=rc.exe
F90=fl32.exe

!IF  "$(CFG)" == "Co2feb26 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
OUTDIR=.
INTDIR=.

ALL : "$(OUTDIR)\Co2feb26.exe"

CLEAN : 
	-@erase ".\Co2feb26.exe"
	-@erase ".\Co2feb26.obj"
	-@erase ".\mo1co2feb.obj"

# ADD BASE F90 /Ox /c /nologo
# ADD F90 /Ox /G5 /c /nologo
F90_PROJ=/Ox /G5 /c /nologo 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Co2feb26.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib naglib.lib /nologo /subsystem:console /machine:I386
# SUBTRACT LINK32 /incremental:yes /debug
LINK32_FLAGS=kernel32.lib naglib.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/Co2feb26.pdb" /machine:I386 /out:"$(OUTDIR)/Co2feb26.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Co2feb26.obj" \
	"$(INTDIR)/mo1co2feb.obj"

"$(OUTDIR)\Co2feb26.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Co2feb26 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
OUTDIR=.
INTDIR=.

ALL : "$(OUTDIR)\Co2feb26.exe"

CLEAN : 
	-@erase ".\Co2feb26.exe"
	-@erase ".\Co2feb26.obj"
	-@erase ".\mo1co2feb.obj"

# ADD BASE F90 /Zi /c /nologo
# ADD F90 /Ox /G5 /Zi /c /nologo
F90_PROJ=/Ox /G5 /Zi /c /nologo /Fd"Co2feb26.pdb" 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Co2feb26.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib naglib.lib /nologo /subsystem:console /incremental:no /machine:I386
# SUBTRACT LINK32 /debug
LINK32_FLAGS=kernel32.lib naglib.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/Co2feb26.pdb" /machine:I386 /out:"$(OUTDIR)/Co2feb26.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Co2feb26.obj" \
	"$(INTDIR)/mo1co2feb.obj"

"$(OUTDIR)\Co2feb26.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for.obj:
   $(F90) $(F90_PROJ) $<  

.f.obj:
   $(F90) $(F90_PROJ) $<  

.f90.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "Co2feb26 - Win32 Release"
# Name "Co2feb26 - Win32 Debug"

!IF  "$(CFG)" == "Co2feb26 - Win32 Release"

!ELSEIF  "$(CFG)" == "Co2feb26 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\Co2feb26.for
DEP_F90_CO2FE=\
	".\mo1co2feb.mod"\
	

"$(INTDIR)\Co2feb26.obj" : $(SOURCE) $(DEP_F90_CO2FE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mo1co2feb.for

!IF  "$(CFG)" == "Co2feb26 - Win32 Release"

F90_MODOUT=\
	"mo1co2feb"


"$(INTDIR)\mo1co2feb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Co2feb26 - Win32 Debug"

F90_MODOUT=\
	"mo1co2feb"


"$(INTDIR)\mo1co2feb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################

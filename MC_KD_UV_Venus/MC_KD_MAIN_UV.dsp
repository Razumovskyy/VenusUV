# Microsoft Developer Studio Project File - Name="MC_KD_MAIN_UV" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=MC_KD_MAIN_UV - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MC_KD_MAIN_UV.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MC_KD_MAIN_UV.mak" CFG="MC_KD_MAIN_UV - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MC_KD_MAIN_UV - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "MC_KD_MAIN_UV - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "MC_KD_MAIN_UV - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x419 /d "NDEBUG"
# ADD RSC /l 0x419 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "MC_KD_MAIN_UV - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x419 /d "_DEBUG"
# ADD RSC /l 0x419 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "MC_KD_MAIN_UV - Win32 Release"
# Name "MC_KD_MAIN_UV - Win32 Debug"
# Begin Source File

SOURCE=.\A_MOD.f90
# End Source File
# Begin Source File

SOURCE=.\CO2_UV_KD.f90
DEP_F90_CO2_U=\
	".\Debug\INITIAL_SHW_KD.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\H2O_UV_KD.f90
DEP_F90_H2O_U=\
	".\Debug\INITIAL_SHW_KD.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\HCL_UV_KD.f90
DEP_F90_HCL_U=\
	".\Debug\INITIAL_SHW_KD.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MC_KD_MAIN_UV.f90
DEP_F90_MC_KD=\
	".\Debug\A_MOD.mod"\
	".\Debug\INITIAL_SHW_KD.mod"\
	".\Debug\M_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MC_RAYL_KD.f90
DEP_F90_MC_RA=\
	".\Debug\A_MOD.mod"\
	".\Debug\INITIAL_SHW_KD.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\OCS_UV_KD.f90
DEP_F90_OCS_U=\
	".\Debug\INITIAL_SHW_KD.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SO2_UV_KD.f90
DEP_F90_SO2_U=\
	".\Debug\INITIAL_SHW_KD.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\UV_5GAS_INITIAL_K_KD.f90
# End Source File
# End Target
# End Project

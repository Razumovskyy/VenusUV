# Microsoft Developer Studio Project File - Name="UV_ETALON_Monte_Carlo" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=UV_ETALON_Monte_Carlo - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "UV_ETALON_Monte_Carlo.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "UV_ETALON_Monte_Carlo.mak" CFG="UV_ETALON_Monte_Carlo - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "UV_ETALON_Monte_Carlo - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "UV_ETALON_Monte_Carlo - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "UV_ETALON_Monte_Carlo - Win32 Release"

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

!ELSEIF  "$(CFG)" == "UV_ETALON_Monte_Carlo - Win32 Debug"

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

# Name "UV_ETALON_Monte_Carlo - Win32 Release"
# Name "UV_ETALON_Monte_Carlo - Win32 Debug"
# Begin Source File

SOURCE=.\K_RABMA_UV_2021_5GAS.f90
DEP_F90_K_RAB=\
	".\Debug\A_MOD_SHW.mod"\
	".\Debug\INITIAL_ShW_CLOUD.mod"\
	".\Debug\MESH.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\M_CARLO_TAU_RaylVenus.f90
DEP_F90_M_CAR=\
	".\Debug\A_MOD_SHW.mod"\
	".\Debug\INITIAL_ShW_CLOUD.mod"\
	".\Debug\M_C.mod"\
	".\Debug\MESH.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MC_ShW.f90
# End Source File
# Begin Source File

SOURCE=.\MODULE_MESH08shw.f90
DEP_F90_MODUL=\
	".\Debug\INITIAL_ShW_CLOUD.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SUN_UV_Venus.f90
# End Source File
# Begin Source File

SOURCE=.\UV_5G.f90
DEP_F90_UV_5G=\
	".\Debug\MESH.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\UV_ETALON_Monte_Carlo.f90
DEP_F90_UV_ET=\
	".\Debug\A_MOD_SHW.mod"\
	".\Debug\INITIAL_ShW_CLOUD.mod"\
	".\Debug\MESH.mod"\
	
# End Source File
# End Target
# End Project

#######################
# MAKEFILE FOR emice.x
# auf sx8.awi.de
# (Architektur und compiler fuer linux IA64)
# frontend uname -a
# Linux sx8 2.6.16.60-0.39.3.PTF.518120.1-default #1 SMP 
# Mon May 11 11:46:34 UTC 2009 ia64 ia64 ia64 GNU/Linux
#######################

#####################
# Compilation flags:

   F90FLAGS = -Popenmp -Ep -Wp "-f -DNEC -DOPERATION" \
		-Wf "-pvctl noassume loopcnt=500000"

		# -Pstatic 
		# -Pstack 
		# -Pmulti 
		# -Popenmp anstatt -mp
		# -C[...]  anstatt -O
		# -ftrace
		# -Wp "... -DOPERATION "
		# -ew	: eight bits numeric storage

   F90LDFLAGS = -Popenmp -Wf "-pvctl noassume loopcnt=500000"

#####################
# Library/Include paths
# AWI sx8
    NETCDFINC = /sx8/user2/awisoft/tx7/netcdf/netcdf-3.6.0-p1/include
    NETCDFLIB = /sx8/user2/awisoft/tx7/netcdf/netcdf-3.6.0-p1/lib
# GEHEN NICHT:
#    NETCDFINC = /sx8/user2/awisoft/sx8/netcdf-4.0/dw/include/
#    NETCDFLIB = /sx8/user2/awisoft/sx8/netcdf-4.0/dw/lib
#    NETCDFINC = /sx8/user2/awisoft/sx8/netcdf-3.6.1/include/
#    NETCDFLIB = /sx8/user2/awisoft/sx8/netcdf-3.6.1/lib
#    NETCDFINC = /sx8/user2/awisoft/sx8/netcdf-3.6.2/include/
#    NETCDFLIB = /sx8/user2/awisoft/sx8/netcdf-3.6.2/lib
# DKRZ
# linuxI64cross
#    NETCDFINC = /pool/SX-6/netcdf/netcdf-3.5.0/include 
#    NETCDFLIB = /pool/SX-6/netcdf/netcdf-3.5.0/lib
# suncross
#   NETCDFINC = /opt/sx/netcdf-3.4/include 
#   NETCDFLIB = /opt/sx/netcdf-3.4/lib

EXE = emice.x

OBJS = \
 FD_EM.main.o \
 MAINEXE.o \
 HANDLE_ERR.o \
 INFIELD.o \
 OUTFIELD.o \
 IOOPT.o \
 OUTTRACE.o \
 PARINPUT.o \
 PHYSNC.o \
 QUELLE.o \
 EMNC.o \
 CalcTimeIndepFieldsForO22andO24.o \
 CalculateHelpFieldsFor2D.o \
 CalculateHelpFieldsFor3D.o  \
 CalculateForO24AbsRange2DTEMode.o \
 CalculateForO24AbsRange1DTEMode.o \
 CalculateForO22AbsRange3D.o \
 CalculateForO24AbsRange3D.o 

#####################
# FORTRAN-QUELLCODES:

# FORTRAN-EXECUTABLE
$(EXE): $(OBJS) 
	sxf90 $(F90LDFLAGS) -o $@ $(OBJS) -L $(NETCDFLIB) -l netcdf

# Turn of existing suffix rules:
.SUFFIXES:;

# Define new suffix rules:
.SUFFIXES: .f .o

.f.o:
	sxf90 $(F90FLAGS) -c -o $*.o $*.f -I $(NETCDFINC) 

MAINEXE.o : FD_EM.main.o

HANDLE_ERR.o : FD_EM.main.o

INFIELD.o : FD_EM.main.o

IOOPT.o : FD_EM.main.o

OUTFIELD.o : FD_EM.main.o

OUTTRACE.o : FD_EM.main.o

PARINPUT.o : FD_EM.main.o

PHYSNC.o : FD_EM.main.o

QUELLE.o: FD_EM.main.o

EMNC.o : FD_EM.main.o

CalcTimeIndepFieldsForO22andO24.o : FD_EM.main.o

CalculateHelpFieldsFor2D.o : FD_EM.main.o

CalculateHelpFieldsFor3D.o : FD_EM.main.o

CalculateForO24AbsRange2DTEMode.o : FD_EM.main.o

CalculateForO24AbsRange1DTEMode.o : FD_EM.main.o

CalculateForO22AbsRange3D.o : FD_EM.main.o

CalculateForO24AbsRange3D.o : FD_EM.main.o

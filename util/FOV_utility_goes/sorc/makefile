SHELL=	/bin/sh
FCOMP  = ifort
FFLAGS = -check all -traceback -fpe0 -ftrapuv -g -r8
EXE= test.exe

OBJS = calc_fov_geo.o \
       kinds.o \
       constants.o \
       test.o

$(EXE):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(EXE) $(OBJS) 

test.o: calc_fov_geo.o constants.o test.f90
	$(FCOMP) $(FFLAGS) -c test.f90
constants.o:  kinds.o constants.f90
	$(FCOMP) $(FFLAGS) -c constants.f90
kinds.o:   kinds.F90
	$(FCOMP) $(FFLAGS) -D_REAL8_ -c kinds.F90
calc_fov_geo.o: constants.o kinds.o calc_fov_geo.f90
	$(FCOMP) $(FFLAGS) -c calc_fov_geo.f90
clean:
	rm -f *.o *.mod $(EXE)

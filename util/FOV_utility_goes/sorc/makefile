SHELL=	/bin/sh
FCOMP  = ifort
FFLAGS = -check all -traceback -fpe0 -ftrapuv -g -r8
EXE= test.exe

OBJS = fov_geo_angle_sizes.o \
       calc_fov_geo.o \
       arc_distance.o \
       kinds.o \
       constants.o \
       fov_ellipse_geo.o \
       inside_fov_geo.o \
       test.o

$(EXE):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(EXE) $(OBJS) 

test.o: constants.o test.f90
	$(FCOMP) $(FFLAGS) -c test.f90
constants.o:  kinds.o constants.f90
	$(FCOMP) $(FFLAGS) -c constants.f90
kinds.o:   kinds.F90
	$(FCOMP) $(FFLAGS) -D_REAL8_ -c kinds.F90
fov_geo_angle_sizes.o: constants.o kinds.o fov_geo_angle_sizes.f90
	$(FCOMP) $(FFLAGS) -c fov_geo_angle_sizes.f90
calc_fov_geo.o: kinds.o calc_fov_geo.f90
	$(FCOMP) $(FFLAGS) -c calc_fov_geo.f90
arc_distance.o: arc_distance.f90
	$(FCOMP) $(FFLAGS) -c arc_distance.f90
fov_ellipse_geo.o: kinds.o fov_ellipse_geo.f90
	$(FCOMP) $(FFLAGS) -c fov_ellipse_geo.f90
inside_fov_geo.o: constants.o kinds.o inside_fov_geo.f90
	$(FCOMP) $(FFLAGS) -c inside_fov_geo.f90
clean:
	rm -f *.o *.mod $(EXE)

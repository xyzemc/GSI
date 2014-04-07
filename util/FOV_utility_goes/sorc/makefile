SHELL=	/bin/sh
FCOMP  = ifort
FFLAGS = -check all -traceback -fpe0 -ftrapuv -g -r8
EXE= test.exe

OBJS = FOVGeoAngleSizes.o \
       FOV_Geo_Ellipse_SAVE.o \
       arc_distance.o \
       FOV_Ellipse_Geo.o \
       Inside_FOV_Ellipse_Flat_Earth_Geo.o \
       test.o

$(EXE):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(EXE) $(OBJS) 

test.o: test.f90
	$(FCOMP) $(FFLAGS) -c test.f90
arc_distance.o:  arc_distance.f90
	$(FCOMP) $(FFLAGS) -c arc_distance.f90
FOVGeoAngleSizes.o: FOVGeoAngleSizes.f90
	$(FCOMP) $(FFLAGS) -c FOVGeoAngleSizes.f90
FOV_Geo_Ellipse_SAVE.o: FOV_Geo_Ellipse_SAVE.f90
	$(FCOMP) $(FFLAGS) -c FOV_Geo_Ellipse_SAVE.f90
FOV_Ellipse_Geo.o: FOV_Ellipse_Geo.f90
	$(FCOMP) $(FFLAGS) -c FOV_Ellipse_Geo.f90
Inside_FOV_Ellipse_Flat_Earth_Geo.o: Inside_FOV_Ellipse_Flat_Earth_Geo.f90
	$(FCOMP) $(FFLAGS) -c Inside_FOV_Ellipse_Flat_Earth_Geo.f90
clean:
	rm -f *.o *.mod $(EXE)

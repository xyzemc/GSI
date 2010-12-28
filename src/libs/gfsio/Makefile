SHELL=/bin/sh

include ../../../configure.gsi

LIB = ../../../lib/libgfsio_i4r4.a


OBJ_F77 = bafrio.o

OBJ = gfsio_module.o

SRC_F77 = $(OBJ_F77:.o=.f)

SRC = $(OBJ:.o=.F90)



$(LIB): $(OBJ) $(OBJ_F77)
	$(CP) *.mod ../../../include
	$(AR) -ruv $(ARFLAGS_GFSIO) $(LIB) $(OBJ) $(OBJ_F77)

$(OBJ):
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) $(CPP_FLAGS) $(CPP_F90FLAGS) $*.b  > $*.f90
	$(SF90) $(FFLAGS_GFSIO) -c $*.f90
	$(RM) $*.f90 $*.b

$(OBJ_F77):
	$(SFC) $(FFLAGS_GFSIO) -c $(SRC_F77)

clean:
	$(RM) $(OBJ) $(OBJ_F77) *.mod $(LIB)


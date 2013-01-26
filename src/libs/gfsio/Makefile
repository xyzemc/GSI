SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libgfsio_i4r4.a
INCMOD  = ../../../include

OBJ_F77 = bafrio.o

OBJ = gfsio_module.o

SRC_F77 = $(OBJ_F77:.o=.f)

SRC = $(OBJ:.o=.F90)



$(LIB): $(OBJ) $(OBJ_F77)
	$(AR) -ruv $(ARFLAGS_GFSIO) $(LIB) $(OBJ) $(OBJ_F77)
	$(CP) *.mod $(INCMOD)

.SUFFIXES:
.SUFFIXES: .fpp .f90 .o

$(OBJ):
	$(CPP) $(CPP_FLAGS) $(CPP_F90FLAGS) $*.F90  > $*.fpp
	$(SF90) $(FFLAGS_GFSIO) -c $*.fpp
	$(RM) $*.fpp

$(OBJ_F77):
	$(SFC) $(FFLAGS_GFSIO) -c $(SRC_F77)

clean:
	$(RM) $(OBJ) $(OBJ_F77) *.mod $(INCMOD)/gfsio_*.mod $(LIB)


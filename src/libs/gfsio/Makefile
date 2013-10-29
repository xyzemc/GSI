SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libgfsio_i4r4.a
INCMOD  = ../../../include


OBJ = gfsio_module.o


SRC = $(OBJ:.o=.F90)



$(LIB): $(OBJ) 
	$(AR) -ruv $(ARFLAGS_GFSIO) $(LIB) $(OBJ)
	$(CP) *.mod $(INCMOD)

.SUFFIXES:
.SUFFIXES: .fpp .f90 .o

$(OBJ):
	$(CPP) $(CPP_FLAGS) $(CPP_F90FLAGS) $*.F90  > $*.fpp
	$(SF90) $(FFLAGS_GFSIO) -c $*.fpp
	$(RM) $*.fpp

clean:
	$(RM) $(OBJ) $(OBJ_F77) *.mod $(INCMOD)/gfsio_*.mod $(LIB)


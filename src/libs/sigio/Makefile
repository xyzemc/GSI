SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libsigio_i4r4.a
INCMOD  = ../../../include

OBJ_F77 = bafrio.o
SRC_F77 = $(OBJ_F77:.o=.f)
OBJ = sigio_module.o sigio_r_module.o
SRC = $(OBJ:.o=.f90)

$(LIB): $(OBJ) $(OBJ_F77)
	$(AR) -ruv $(ARFLAGS_SIGIO) $(LIB) $(OBJ) $(OBJ_F77)
	$(CP) *.mod $(INCMOD)

.SUFFIXES:
.SUFFIXES:  .f .f90 .o

.f.o:
	$(SFC) $(FFLAGS_SIGIO) -c $(SRC_F77)

.f90.o:
	$(SF90) $(FFLAGS_SIGIO) -c $(SRC)

clean:
	$(RM) $(OBJ) $(OBJ_F77) $(LIB) *.mod $(INCMOD)/sigio_*.mod

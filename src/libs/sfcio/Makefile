SHELL=/bin/sh
include ../../../configure.gsi
LIB = ../../../lib/libsfcio_i4r4.a

OBJ = sfcio_module.o
SRC = $(OBJ:.o=.f90)

$(LIB): $(OBJ)
	$(CP) sfcio_module.mod ../../../include/sfcio_module.mod
	$(AR) -ruv $(ARFLAGS_SFCIO) $(LIB) $(OBJ)

.f90.o:
	$(SF90) $(FFLAGS_SFCIO) -c $(SRC)

clean:
	$(RM) $(OBJ) $(LIB) *.mod ../../../include/sfcio_module.mod


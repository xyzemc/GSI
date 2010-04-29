SHELL=/bin/sh
 
include ../../../configure.gsi

# LIB = ../../../lib/libcrtm_gfsgsi.a
LIB = libcrtm_gfsgsi.a

include make.filelist
 
.SUFFIXES: .f90 .o

.f90.o:
	$(SF90) $(FFLAGS) -c $<


all:	$(LIB)

$(LIB):	$(OBJ_FILES)
	$(AR) crvs $(LIB) $(OBJ_FILES)

.f90.a:
	$(SF90) -c $(FFLAGS) $<

.c.a:
	$(SCC) -c $(CFLAGS) $<

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include make.dependencies

clean:
	rm -f *.o *.mod $(LIB)


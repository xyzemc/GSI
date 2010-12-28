SHELL=/bin/sh
 
include ../../../configure.gsi

LIB = ../../../lib/libcrtm.a

include make.filelist
 

.SUFFIXES: .f90 .o

all:	$(LIB)

$(LIB):	$(OBJ_FILES)
	$(CP) *.mod ../../../include
	$(AR) crvs $(LIB) $(OBJ_FILES)

.f90.o:
	$(SF90) -c $(FFLAGS_CRTM) $(LFLAGS_CRTM) $<

clean:
	$(RM) -f $(OBJ_FILES) *.mod $(LIB)

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include make.dependencies



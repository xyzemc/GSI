SHELL=/bin/sh

LIB = ../../../lib/libcrtm_gfsgsi.a

include make.filelist

all:	$(LIB)

$(LIB):	$(OBJ_FILES)
	$(AR) -ruv $(LIB) $(OBJ_FILES)


.f.a:
	$(FC) -c $(FFLAGS) $<
#	ar -ruv $(AFLAGS) $@ $*.o
#	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
#	ar -ruv  $(AFLAGS) $@ $*.o
#	rm -f $*.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include make.dependencies

clean:
	rm -f *.o *.mod $(LIB)


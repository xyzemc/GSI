SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libbacio.a
INCMOD  = ../../../include

INC=clib4.h

$(LIB):	$(LIB)(bacio.v1.4.o baciof.o bafrio.o)

$(LIB)(bacio.v1.4.o):       bacio.v1.4.c $(INC)
	ln -fs $(INC) clib.h
	$(SCC) -c $(CFLAGS2) bacio.v1.4.c
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) bacio.v1.4.o

$(LIB)(baciof.o):   baciof.f
	$(SFC) -c $(FFLAGS_BACIO) baciof.f
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) baciof.o 
	$(CP) *.mod $(INCMOD)

$(LIB)(bafrio.o):   bafrio.f
	$(SFC) -c $(FFLAGS_BACIO) bafrio.f
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) bafrio.o 

clean:
	$(RM) baciof.o bafrio.o clib.h $(LIB) bacio.v1.4.o *.mod $(INCMOD)/bacio_module.mod


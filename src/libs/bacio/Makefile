SHELL=/bin/sh

include ../../../configure.gsi

LIB=../../../lib/libbacio.a

INC=clib4.h
## CFLAGS= -O3 -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' 

$(LIB):	$(LIB)( bacio.v1.4.o baciof.o bafrio.o )

$(LIB)(bacio.v1.4.o):       bacio.v1.4.c $(INC)
	ln -fs $(INC) clib.h
	$(CC) -c $(CFLAGS2) bacio.v1.4.c
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) bacio.v1.4.o

$(LIB)(baciof.o):   baciof.f
	$(FC) -c $(FFLAGS_BACIO) baciof.f
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) baciof.o 

$(LIB)(bafrio.o):   bafrio.f
	$(FC) -c $(FFLAGS_BACIO) bafrio.f
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) bafrio.o 

clean:
	$(RM) baciof.o bafrio.o clib.h $(LIB) bacio.v1.4.o bacio_module.mod

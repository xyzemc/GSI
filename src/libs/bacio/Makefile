SHELL=/bin/sh

LIB=../../../lib/libbacio.a
INC=clib4.h
FFLAGS= -O3
AFLAGS=
## CFLAGS= -O3 -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' 

$(LIB):	$(LIB)( bacio.v1.4.o baciof.o bafrio.o )

$(LIB)(bacio.v1.4.o):       bacio.v1.4.c $(INC)
	ln -fs $(INC) clib.h
	$(CC) -c $(CFLAGS2) bacio.v1.4.c
	ar -rv $(AFLAGS) $(LIB) bacio.v1.4.o
#	rm clib.h

$(LIB)(baciof.o):   baciof.f
	$(FC) -c $(FFLAGS) baciof.f
	ar -rv $(AFLAGS) $(LIB) baciof.o 

$(LIB)(bafrio.o):   bafrio.f
	$(FC) -c $(FFLAGS) bafrio.f
	ar -rv $(AFLAGS) $(LIB) bafrio.o 
	rm -f baciof.o bafrio.o clib.h

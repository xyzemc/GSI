SHELL=/bin/sh
include ../../../configure.gsi
LIB     = ../../../lib/libnemsio.a
INCMOD  = ../../../include
## FC      = xlf95_r
## FFLAGS  = -O3
## AR      = ar
## ARFLAGS = -rv -X64

MODULES  = nemsio_openclose.o nemsio_read.o nemsio_write.o nemsio_module.o

$(LIB): $(MODULES)
	$(AR) -ruv $@ $(MODULES)
	mv *.mod $(INCMOD)

clean:
	rm -f *.o *.mod $(LIB)
	
.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(F90) -c $*.f90

nemsio_read.o: nemsio_openclose.o
nemsio_write.o: nemsio_openclose.o
nemio_module.o: nemsio_openclose.o nemsio_read.o nemsio_write.o
nemio_gfs.o: nemsio_module.o

SHELL=/bin/sh

LIB = ../../../lib/libgfsio.a

.SUFFIXES: .f90 .o

.f90.o:
##	$(RM) $@ $*.mod
	$(F90) $(FFLAGS) -c $<

$(LIB): gfsio_module.o bafrio.o
	$(AR) -ruv $(LIB) gfsio_module.o bafrio.o


.f90.a:
	$(F90) -c -qfree  $(FFLAGS) $<
	ar -ruv $(AFLAGS) $@ $*.o
	rm -f $*.o

.f.a:
	$(F90) -c -qfree  $(FFLAGS) $<
	ar -ruv $(AFLAGS) $@ $*.o
	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
	ar -ruv  $(AFLAGS) $@ $*.o
	rm -f $*.o

clean:
	rm -f *.o *.mod $(LIB)


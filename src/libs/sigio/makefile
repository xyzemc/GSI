SHELL=/bin/sh

LIB = ../../../lib/libsigio.a

$(LIB): $(LIB)(sigio_module.o sigio_r_module.o bafrio.o)


$(LIB)(bafrio.o):       bafrio.f
	$(FC) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	rm -f $*.o

.f.a:
	$(F90) -c $(FFLAGS) $<
	ar -ruv $(AFLAGS) $@ $*.o
	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
	ar -ruv  $(AFLAGS) $@ $*.o
	rm -f $*.o

clean:
	rm -f *.o *.mod $(LIB)


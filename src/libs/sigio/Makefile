SHELL=/bin/sh

include ../../../configure.gsi

LIB = ../../../lib/libsigio.a

$(LIB): $(LIB)(sigio_module.o sigio_r_module.o bafrio.o)


$(LIB)(bafrio.o):       bafrio.f
	$(FC) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	rm -f $*.o

.f.a:
	$(F90) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	$(RM) $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	$(RM) $*.o

clean:
	$(RM) *.o *.mod $(LIB)


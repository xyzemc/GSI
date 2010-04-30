SHELL=/bin/sh

include ../../../configure.gsi

LIB = ../../../lib/libsfcio.a

$(LIB): $(LIB)(sfcio_module.o )


.f.a:
	$(F90) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	$(RM) $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
	$(AR) $(ARFLAGS) $(AFLAGS) $@ $*.o
	$(RM) $*.o

clean:
	$(RM) *.o *.mod $(LIB)


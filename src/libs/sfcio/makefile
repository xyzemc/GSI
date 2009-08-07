SHELL=/bin/sh

LIB = ../../../lib/libsfcio.a

$(LIB): $(LIB)(sfcio_module.o )


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


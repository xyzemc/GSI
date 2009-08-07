SHELL=/bin/sh

LIB = ../../../lib/libbacio.a

$(LIB): $(LIB)(  bacio.v1.3.o baciof.o )

.f.a:
	$(FC) -c -I . $(FFLAGS) $<
	ar -ruv $(AFLAGS) $@ $*.o
	rm -f $*.o

.c.a:
	$(CC) -c -I . $(CFLAGS) $<
	ar -ruv  $(AFLAGS) $@ $*.o
	rm -f $*.o

clean:
	rm -f *.o *.mod $(LIB)


SHELL=/bin/sh

include ../../../configure.gsi

LIB = ../../../lib/libgfsio.a

OBJS = bafrio.o

OBJS1 = gfsio_module.o

SRC_F77 = $(OBJS:.o=.f)

SRC_F = $(OBJS1:.o=.F)



$(LIB): $(OBJS)
	$(F90) $(FFLAGS) -c $(SRC_F)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS) $(OBJS1)

.f.o:
	$(FC) $(FFLAGS) -c $(SRC_F77)

clean:
	$(RM) $(OBJS) $(OBJS1) *.mod $(LIB)


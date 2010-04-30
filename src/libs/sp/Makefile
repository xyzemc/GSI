SHELL=/bin/sh

include ../../../configure.gsi

LIB = ../../../lib/libsp.a 


OBJS = bll2ps.o spanaly.o spdz2uv.o speps.o spfft.o spfft1.o \
spfftpt.o spgradq.o spgradx.o spgrady.o splaplac.o \
splegend.o sppad.o spsynth.o sptez.o sptezd.o sptezm.o sptezmd.o sptezmv.o \
sptezv.o sptgpm.o sptgpmd.o sptgpmv.o sptgps.o sptgpsd.o sptgpsv.o sptgpt.o \
sptgptd.o sptgptsd.o sptgptv.o sptgptvd.o sptran.o sptrand.o sptranf.o sptranf0.o \
sptranf1.o sptranfv.o sptranv.o sptrun.o sptrund.o sptrung.o sptrungv.o sptrunl.o \
sptrunm.o sptrunmv.o sptruns.o sptrunsv.o sptrunv.o spuv2dz.o spvar.o spwget.o

OBJS1 = fftpack.o ncpus.o spffte.o splat.o

SRC_F77 = $(OBJS:.o=.f)

SRC_F = $(OBJS1:.o=.F)



$(LIB): $(OBJS)
	$(FC) $(FFLAGS) $(FFLAGS_DOUBLE) -c $(SRC_F) $(SRC_F77)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS) $(OBJS1)

clean:
	$(RM) $(OBJS) $(OBJS1) *.mod $(LIB)


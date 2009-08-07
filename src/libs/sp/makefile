SHELL=/bin/sh

LIB = ../../../lib/libsp.a

$(LIB): $(LIB)( bll2ps.o ncpus.o spanaly.o spdz2uv.o speps.o spfft.o spfft1.o spffte.o spfftpt.o spgradq.o spgradx.o spgrady.o splaplac.o splat.o splegend.o sppad.o spsynth.o sptez.o sptezd.o sptezm.o sptezmd.o sptezmv.o sptezv.o sptgpm.o sptgpmd.o sptgpmv.o sptgps.o sptgpsd.o sptgpsv.o sptgpt.o sptgptd.o sptgptsd.o sptgptv.o sptgptvd.o sptran.o sptrand.o sptranf.o sptranf0.o sptranf1.o sptranfv.o sptranv.o sptrun.o sptrund.o sptrung.o sptrungv.o sptrunl.o sptrunm.o sptrunmv.o sptruns.o sptrunsv.o sptrunv.o spuv2dz.o spvar.o spwget.o)


.f.a:
	$(FC) -c $(FFLAGS) $<
	ar -ruv $(AFLAGS) $@ $*.o
	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
	ar -ruv  $(AFLAGS) $@ $*.o
	rm -f $*.o

clean:
	rm -f *.o *.mod $(LIB)


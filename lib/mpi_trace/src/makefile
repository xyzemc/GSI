ARFLAGS = -X32_64 

OPT1 = -c -g -O -qmaxmem=-1 -qinfo=pro:gen
OPT3 = -c -O3 -qinfo=pro:gen

#=========================================
# use this target if you have hpm + power5
#=========================================
 all : libmpitrace.a libmpihpm.a  libsummary.a \
       libmpitrace_smp.a libmpihpm_smp.a clean_obj
	mv *.a ../lib

#=========================================
# use this target if you don't
#=========================================
#all : libmpitrace.a libmpitrace_smp.a clean_obj
#	mv *.a ../lib

#-----------------------------------------
TRCO = mpitrace32.o mpitrace64.o mpitrace_caller32.o mpitrace_caller64.o mpitrace_sortx32.o mpitrace_sortx64.o

TRCO_SMP = mpitrace32_smp.o mpitrace64_smp.o   mpitrace_caller32.o mpitrace_caller64.o mpitrace_sortx32.o mpitrace_sortx64.o

TRHO = mpitrace_hpm32.o mpitrace_hpm64.o  mpitrace_caller32.o mpitrace_caller64.o mpitrace_sortx32.o mpitrace_sortx64.o

TRHO_SMP = mpitrace_hpm32_smp.o mpitrace_hpm64_smp.o  mpitrace_caller32.o mpitrace_caller64.o mpitrace_sortx32.o mpitrace_sortx64.o

HPMO = mpihpm32.o mpihpm64.o

SUMO = timing_wrappers32.o timing_wrappers64.o
#-----------------------------------------
mpihpm32.o : mpihpm.c
	mpcc_r -q32 $(OPT1) mpihpm.c -o mpihpm32.o

mpihpm64.o : mpihpm.c
	mpcc_r -q64 $(OPT1) mpihpm.c -o mpihpm64.o

mpitrace32.o : mpitrace.c
	mpcc_r -q32 $(EXTNAME) $(OPT1) mpitrace.c -o mpitrace32.o

mpitrace32_smp.o : mpitrace.c
	mpcc_r -q32 -qsmp=omp $(EXTNAME) $(OPT1) mpitrace.c -o mpitrace32_smp.o

mpitrace64.o : mpitrace.c
	mpcc_r -q64 $(EXTNAME) $(OPT1) mpitrace.c -o mpitrace64.o

mpitrace64_smp.o : mpitrace.c
	mpcc_r -q64 -qsmp=omp $(EXTNAME) $(OPT1) mpitrace.c -o mpitrace64_smp.o

mpitrace_hpm32.o : mpitrace.c
	mpcc_r -q32 $(EXTNAME) -DHPM $(OPT1) mpitrace.c -o mpitrace_hpm32.o

mpitrace_hpm32_smp.o : mpitrace.c
	mpcc_r -q32 -qsmp=omp $(EXTNAME) -DHPM $(OPT1) mpitrace.c -o mpitrace_hpm32_smp.o

mpitrace_hpm64.o : mpitrace.c
	mpcc_r -q64 $(EXTNAME) -DHPM $(OPT1) mpitrace.c -o mpitrace_hpm64.o

mpitrace_hpm64_smp.o : mpitrace.c
	mpcc_r -q64 -qsmp=omp $(EXTNAME) -DHPM $(OPT1) mpitrace.c -o mpitrace_hpm64_smp.o

mpitrace_sortx32.o : mpitrace_sortx.c
	cc_r -q32 $(OPT3) mpitrace_sortx.c -o mpitrace_sortx32.o

mpitrace_sortx64.o : mpitrace_sortx.c
	cc_r -q64 $(OPT3) mpitrace_sortx.c -o mpitrace_sortx64.o

mpitrace_caller32.o : mpitrace_caller.c
	cc_r -q32 $(OPT1) mpitrace_caller.c -o mpitrace_caller32.o

mpitrace_caller64.o : mpitrace_caller.c
	cc_r -q64 $(OPT1) mpitrace_caller.c -o mpitrace_caller64.o

timing_wrappers32.o : timing_wrappers.c
	mpcc_r -q32 $(OPT1) timing_wrappers.c -o timing_wrappers32.o

timing_wrappers64.o : timing_wrappers.c
	mpcc_r -q64 $(OPT1) timing_wrappers.c -o timing_wrappers64.o

#-----------------------------------------
libmpitrace.a :$(TRCO)
	rm -f libmpitrace.a
	ar $(ARFLAGS) rv libmpitrace.a $(TRCO)

#-----------------------------------------
libmpitrace_smp.a :$(TRCO_SMP)
	rm -f libmpitrace_smp.a
	ar $(ARFLAGS) rv libmpitrace_smp.a $(TRCO_SMP)

#-----------------------------------------
libmpihpm.a : $(HPMO) $(TRHO)
	rm -f libmpihpm.a
	ar $(ARFLAGS) rv libmpihpm.a $(HPMO) $(TRHO)

#-----------------------------------------
libmpihpm_smp.a : $(HPMO) $(TRHO_SMP)
	rm -f libmpihpm_smp.a
	ar $(ARFLAGS) rv libmpihpm_smp.a $(HPMO) $(TRHO_SMP)
#-----------------------------------------
libsummary.a : $(HPMO) $(SUMO)
	rm -f libsummary.a
	ar $(ARFLAGS) rv libsummary.a $(HPMO) $(SUMO)

#-----------------------------------------
clean_obj :
	rm -f *.o

#-----------------------------------------
clean :
	rm -f *.o *.a

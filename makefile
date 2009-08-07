#SHELL=/bin/sh
#

include ./configure.gsi


#SUBDIRS = sorc/lib sorc/main
SUBDIRS = src

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make FC="$(DM_FC)" F90="$(DM_F90)"  \
		FFLAGS="$(FFLAGS)" CC="$(SCC)" CFLAGS="$(CFLAGS)" \
		CPPFLAGS="$(CPPFLAGS)" CPP="$(CPP)" \
		NETCDFPATH="$(NETCDFPATH)" NETCDFLIBS="$(NETCDFLIBS)" \
		WRF_LIB="$(WRF_LIB)" WRFFLAGS="$(WRF_INCLUDE)"  \
		AFLAGS="$(AFLAGS)"  );  \
        done

clean: $(SUBDIRS)
	echo "subdirs is: $(SUBDIRS)"
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make $@) ; \
        done

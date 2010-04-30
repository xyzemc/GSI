SHELL=/bin/sh

SUBDIRS = libs

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make FC="$(FC)" F90="$(F90)" FFLAGS="$(FFLAGS)" FFLAGS_DOUBLE="$(FFLAGS_DOUBLE)" CC="$(CC)" CFLAGS="$(CFLAGS)"  CFLAGS2="$(CFLAGS2)" CPPFLAGS="$(CPPFLAGS)" CPP="$(CPP)" SED_FTN="$(SED_FTN)" AFLAGS="$(AFLAGS)" );  \
        done
	( cd main; echo "Making $@ in `pwd`" ; \
                make -f makefile_DTC FC="$(FC)" F90="$(F90)" FFLAGS="$(FFLAGS)" FFLAGS_DOUBLE="$(FFLAGS_DOUBLE)" CC="$(CC)" CFLAGS="$(CFLAGS)"  CFLAGS2="$(CFLAGS2)" CPPFLAGS="$(CPPFLAGS)" CPP="$(CPP)" SED_FTN="$(SED_FTN)" AFLAGS="$(AFLAGS)" ); 

clean: $(SUBDIRS) clean_main
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make $@ ) ; \
        done

clean_main:
	( cd main; echo "Making $@ in `pwd`" ; make -f makefile_DTC clean ) ;

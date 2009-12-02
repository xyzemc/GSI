SHELL=/bin/sh

SUBDIRS = bacio bufr crtm_gfsgsi gfsio mpeu sfcio sigio sp w3

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make FC="$(FC)" F90="$(F90)" FFLAGS="$(FFLAGS)" CC="$(CC)" CFLAGS="$(CFLAGS)" CFLAGS2="$(CFLAGS2)" CPPFLAGS="$(CPPFLAGS)" CPP="$(CPP)"   \
		SED_FTN="$(SED_FTN)" AFLAGS="$(AFLAGS)"  );  \
        done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make $@ ) ; \
        done

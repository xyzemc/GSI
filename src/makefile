SHELL=/bin/sh

SUBDIRS = libs main

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make FC="$(FC)" F90="$(F90)" FFLAGS="$(FFLAGS)" CC="$(CC)" CFLAGS="$(CFLAGS)" CPPFLAGS="$(CPPFLAGS)" CPP="$(CPP)" AFLAGS="$(AFLAGS)" );  \
        done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make $@ ) ; \
        done

SHELL=/bin/sh

include ../../configure.gsi

SUBDIRS = bacio bufr crtm_jcsda_2.0 sp gfsio sfcio sigio w3 gsdcloud

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make CPP="$(CPP)"   \
		SED_FTN="$(SED_FTN)"  );  \
        done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make $@ ) ; \
        done

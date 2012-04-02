SHELL=/bin/sh

SUBDIRS = bacio bufr crtm_jcsda_2.0 sp gfsio sfcio sigio w3 gsdcloud nemsio

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make ) ; \
        done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make $@ ) ; \
        done

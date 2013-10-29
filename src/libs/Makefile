SHELL=/bin/sh

SUBDIRS = bacio bufr sp gfsio sfcio sigio w3 gsdcloud nemsio misc crtm_2.1.3

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make ) ; \
        done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make $@ ) ; \
        done

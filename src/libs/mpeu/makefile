SHELL=/bin/sh

include ../../../configure.gsi

LIB = ../../../lib/libmpeu.a

OBJS = m_realkinds.o \
        m_intkinds.o \
        m_chars.o \
        m_stdio.o \
        m_mpif.o \
        m_realkinds.o \
	m_mpif90.o \
	m_die.o                     \
	m_flow.o                    \
	get_zeits.o \
	m_mpif.o \
	m_FileResolv.o \
	m_IndexBin_char.o           \
	m_IndexBin_integer.o        \
	m_IndexBin_logical.o        \
	m_List.o                    \
	m_MergeSorts.o              \
	m_Filename.o                \
	m_Group.o                   \
	m_Permuter.o                \
	m_SortingTools.o            \
	m_String.o                  \
	m_StrTemplate.o             \
	m_dropdead.o                \
	m_inpak90.o                 \
	m_ioutil.o                  \
	m_mall.o                    \
	m_mergedList.o              \
	m_mpout.o                   \
	m_rankMerge.o               \
	m_realkinds.o               \
	m_showDistrib.o             \
	m_zeit.o                    

#m_random.o \

all:	$(LIB)


$(LIB):	$(OBJS)
	echo $(F90)
	$(AR) -ruv $(LIB) $(OBJS)


.f90.a:
	$(F90) -c $(FFLAGS) $<
#	ar -ruv $(AFLAGS) $@ $*.o
#	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
#	ar -ruv  $(AFLAGS) $@ $*.o
#	rm -f $*.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include make.dependencies

clean:
	rm -f *.o *.mod $(LIB)



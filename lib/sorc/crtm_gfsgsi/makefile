#==============================================================================
#
# Makefile for JCSDA CRTM Build
#
# $Id: makefile,v 1.10 2006/08/31 17:06:47 frpv Exp $
#
#==============================================================================

# -------------
# Define macros
# -------------
include ./make.macros

# This makefile
MAKE_FILE = makefile

# The library name
LIBRARY_TAG = crtm_gfsgsi
LIBRARY_NAME = lib$(LIBRARY_TAG).a

# The library directories
SOURCE_DIR = ./
LIBRARY_DIR = ../../
INCLUDE_DIR = ../../incmod/$(LIBRARY_TAG)

# The filelist
include make.filelist


# ------------------
# Define dependecies
# ------------------
# Default build based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKE_FILE) library $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) ibm ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) library $(IRIX64_FLAGS) ;; \
	  "Linux"|"Darwin") make -f $(MAKE_FILE) library $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac

# IBM AIX Compiler
ibm_debug:
	make -f $(MAKE_FILE) library $(AIX_FLAGS_DEBUG)

ibm:
	make -f $(MAKE_FILE) library $(AIX_FLAGS_PROD)
	make -f $(MAKE_FILE) install


# SGI MIPSpro Compiler
sgi_debug:
	make -f $(MAKE_FILE) library $(IRIX64_FLAGS_DEBUG)

sgi:
	make -f $(MAKE_FILE) library $(IRIX64_FLAGS_PROD)


# SunOS Compiler
sun_debug:
	make -f $(MAKE_FILE) library $(SUNOS_FLAGS_DEBUG)

sun:
	make -f $(MAKE_FILE) library $(SUNOS_FLAGS_PROD)


# Linux compilers.
intel_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_INTEL_DEBUG)

intel:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_INTEL_PROD)

lahey_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_LAHEY_DEBUG)

lahey:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_LAHEY_PROD)

pgi_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_PGI_DEBUG)

pgi:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_PGI_PROD)

g95_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_G95_DEBUG)

g95:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_G95_PROD)


# Build the library and include files
library: $(OBJ_FILES)
	$(ARCHIVER) $(ARCHIVER_FLAGS) $(LIBRARY_NAME) $(OBJ_FILES)

# Clean up the chaff
clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb *.a

# Define build dependencies and rules
include make.dependencies
include make.rules



# ---------------------------------------------------------------------
# Install the library and include files in their respective directories
# ---------------------------------------------------------------------
install: install_lib install_include

install_lib:
	@if [ ! -d $(LIBRARY_DIR) ]; then \
	  mkdir -p $(LIBRARY_DIR); \
	fi; \
	cd $(SOURCE_DIR); \
	if [ -f $(LIBRARY_NAME) ]; then \
	  $(MOVE) $(LIBRARY_NAME) $(LIBRARY_DIR); \
	fi ; \
	cd ..

install_include:
	@if [ ! -d $(INCLUDE_DIR) ]; then \
	  mkdir -p $(INCLUDE_DIR); \
	fi; \
	cd $(SOURCE_DIR); \
	for FILE in `ls -1 | egrep "\.(mod|MOD|m|M)$$"`; do \
	  if [ -f $$FILE ]; then \
	    $(COPY) -f $$FILE $(INCLUDE_DIR); \
	  fi ; \
	done ; \
	cd ..


# --------
# Clean up
# --------
# Just the source directory
clean:
	@cd $(SOURCE_DIR); make clean; cd ..


# Everything
distclean: clean clean_lib clean_include

clean_lib:
	$(REMOVE)r $(LIBRARY_DIR)

clean_include:
	$(REMOVE)r $(INCLUDE_DIR)

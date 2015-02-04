#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([COMMUNITY-GSI], [3.3], [gsi-help@ucar.edu])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([foreign dist-bzip2])

# Required influential environment variables
AC_ARG_VAR([NETCDF], [The absolute path where NetCDF libraries and include files exist.])
AC_ARG_VAR([WRF_DIR], [The absolute path where WRF is built.])

# Do we want debug mode or not?
AC_ARG_ENABLE([debug],[AS_HELP_STRING([--enable-debug],[Enable debug mode])],
              [:],[enable_debug=check])

# Test required NETCDF env var
if test "x${NETCDF}" = "x" ; then
   AC_MSG_ERROR([NETCDF environment varialbe is not set. Please set it as an environment variable or run configure with "NETCDF=<path>" as a command-line option.])   
fi

# Test require WRF_DIR env var
if test "x${WRF_DIR}" = "x" ; then
   AC_MSG_ERROR([WRF_DIR environment varialbe is not set. Please set it as an environment variable or run configure with "WRF_DIR=<path>" as a command-line option.])   
fi

# Test if libraries need to be build
if test "x${CORELIB}" = "x"; then
   AC_MSG_NOTICE([CORELIB environment variable is not set. Required libraries will be built.])
   build_libs=true    # can only be true (TRUE) or false (FALSE) not yes or no
   CORELIB='$(abs_top_srcdir)/lib'
else
   AC_MSG_NOTICE([CORELIB environment variable is set. Use libraries in $CORELIB for linking])
   build_libs=false   # can only be true (TRUE) or false (FALSE) not yes or no
fi
AM_CONDITIONAL([BUILDLIBS], [test x${build_libs} = xtrue])

# Keep libtool macros in an m4 directory.
AC_CONFIG_MACRO_DIR([m4])

# Find out about the host we're building on.
AC_CANONICAL_HOST

# For the purpose of zeus and wcoss
hostname=`uname -n`
#AM_CONDITIONAL([ZEUS], [test "${hostname}" = "zeus.rdhpcs.noaa.gov"])
#AM_CONDITIONAL([WCOSS],[test "${hostname}" = "wcoss.rdhpcs.noaa.gov"])
AM_CONDITIONAL([NOAAHPC],[test "${hostname}" = "zeus.rdhpcs.noaa.gov" -o "${hostname}" = "wcoss.rdhpcs.noaa.gov"])

AC_CONFIG_FILES([makefile
                 src/makefile
                 src/libs/makefile
                 src/libs/bacio/makefile
                 src/libs/bufr/makefile
                 src/libs/crtm_2.1.3/makefile
                 src/libs/gsdcloud/makefile
                 src/libs/misc/makefile
                 src/libs/nemsio/makefile
                 src/libs/sfcio/makefile
                 src/libs/sigio/makefile
                 src/libs/sp/makefile
                 src/libs/w3emc_v2.0.5/makefile
                 src/libs/w3nco_v2.0.6/makefile
                 src/main/makefile])

# Checks for programs.
AC_PROG_CC(gcc icc pgccc)
AC_PROG_MAKE_SET
AM_PROG_CC_C_O

# Use libtool
LT_INIT([disable-shared])
AC_SUBST([LIBTOOL_DEPS])

# If the user set environment var FC, but not F77, then set F77=FC
if test "x${FC+set}" = xset -a "x${F77+set}" != xset; then
   F77=${FC}
fi

# If F90 is set, and FC isn't, then set FC to whatever F90 was set to.
if test "${F90+set}" = set -a "${FC+set}" != set; then
   FC=$F90
fi

# If F77 is set, and FC (still) isn't, then set FC to whatever F77 was
# set to.
if test "${F77+set}" = set -a "${FC+set}" != set; then
   FC=$F77
fi

# Find fortran compiler. If FC isn't set after AC_PROG_FC, we couldn't
# find a fortran compiler. If the user requested the fortran API, and
# has told us not to recover from a missing compiler, then bail out
# right here.
AC_MSG_NOTICE([finding Fortran compiler (will not be used if Fortran API is not desired)])
AC_PROG_FC

AC_PROG_F77($FC)

# Handle missing f77 compiler.
if test "${F77+set}" = set && test "x$F77" = x; then
   AC_MSG_ERROR([Can't find F77 compiler.])
fi

pgi_version=`$FC -V | grep pgf | awk '{print $2}'`
echo $pgi_version > pgi_version.txt

AC_CHECK_PROGS(MPIFC, mpif90 hf90 mpxlf90 mpxlf95 mpf90 mpfort cmpifc cmpif90c, $FC)
#echo $MPIFC > mpifc.txt
MPIFC_FULLPATH=`which $MPIFC`
MPI_BIN=`dirname $MPIFC_FULLPATH`
MPI_INCLUDE=`echo $MPI_BIN | sed 's/bin$/include/'`

#     AC_ARG_WITH(mpi, [AS_HELP_STRING([--with-mpi],
#         [compile with MPI (parallelization) support. If none is found,
#         MPI is not used. Default: auto])
#     ],,[with_mpi=auto])
#
#     AX_PROG_FC_MPI([test x"$with_mpi" != xno[,use_mpi=yes[,
#       use_mpi=no
#       if test x"$with_mpi" = xyes; then
#         AC_MSG_FAILURE([MPI compiler requested, but couldn't use MPI.])
#       else
#         AC_MSG_WARN([No MPI compiler found, won't use MPI.])
#       fi
#     ]]])


# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_CHECK_FUNCS([memmove memset])

#
CPP=cpp
AC_SUBST(CPP)

CFLAGS=""
FFLAGS=""
FCFLAGS=""
CPPFLAGS=""

SFC="$FC"
SCC="$CC"

case $FC in
  pgf*)
    case $host_cpu in
      *64*)
        world_size_FFLAGS="-pc 64"
        ;;
      *)
        world_size_FFLAGS="-pc 32"
        ;;
    esac

    case $CC in
      gcc)
        DM_CC="mpicc"
        ;;
      pgcc)
        DM_CC="mpicc"
        ;;
    esac

    FFLAGS_DEFAULT="-Kieee -Ktrap=fp -byteswapio"
    if test "$enable_debug" = yes; then
       optimization_FFLAGS="-O0 -q -C"
    else
       optimization_FFLAGS="-fast"
    fi

    LDFLAGS_ARCH="-Wl,-noinhibit-exec"
    FFLAGS_OUTPUT="$optimization_FFLAGS \$(FFLAGS_DEFAULT) -DLANGUAGE_FORTRAN -DsysLinux \$(INC_FLAGS) \$(LDFLAGS) -DLINUX -DPGI"
    SF90="$FC -Mfree"
    I4R4="-i4 -r4"
    I4R8="-i4 -r8"
    I8R8="-i8 -r8"
    CRTM_OPT="-fast"
    CPP_FLAGS="-C -P -D_REAL8_ -DWRF -DLINUX -DPGI"
    FFLAGS_DEBUG="-O0 -q -C"

    DM_FC="mpif90"
    DM_F90="mpif90 -Mfree"

    MYLIBsys="-llapack -lblas"
    NETCDF_PATH="-rpath,$NETCDF/lib"

    ;;
  ifort)
    AC_ARG_VAR([LAPACK_PATH], [The absolute path of the LAPACK math library. The library is required when building with Intel ifort. Typically the required path is available through a system variable such as \$MKLROOT.])

   # Test require LAPACK_PATH env var
   if test "x${LAPACK_PATH}" = "x" ; then
      AC_MSG_ERROR([LAPACK_PATH environment varialbe is not set. It's the absolute path of the LAPACK math library. The library is required when building with Intel ifort. Typically the required path is available through a system variable such as \$MKLROOT. Please set it as an environment variable or run configure with "LAPACK_PATH=<path>" as a command-line option.])   
   fi

    world_size_FFLAGS=
   #FFLAGS_DEFAULT="-traceback -convert big_endian -assume byterecl -g -fp-model strict"
    FFLAGS_DEFAULT="-fp-model precise -assume byterecl -convert big_endian"
    if test "$enable_debug" == "yes"; then
       optimization_FFLAGS="-O0 -check bounds -fpe0 -ftz -warn errors -fpstkchk -mp  -fp-model precise -implicitnone -debug -ftrapuv -check all  -fp-stack-check  -fstack-protector -warn"
    else
       optimization_FFLAGS="-O3"
    fi

    LDFLAGS_ARCH="-openmp"
    FFLAGS_OUTPUT="$optimization_FFLAGS \$(FFLAGS_DEFAULT) \$(INC_FLAGS) \$(LDFLAGS) -DLINUX"
    SF90="$FC -free"

    I4R4="-integer-size 32 -real-size 32"
    I4R8="-integer-size 32 -real-size 64"
    I8R8="-integer-size 64 -real-size 64"
    CRTM_OPT="-O2"
    CPP_FLAGS="-C -P -D_REAL8_ -DWRF -DLINUX"
    FFLAGS_DEBUG="-O0 -g -traceback -check bounds -fpe0 -ftz -warn errors -fpstkchk -mp"

    DM_CC="$CC"
    DM_FC="mpif90  -f90=$SFC"
    DM_F90="mpif90 -free -f90=$SFC"

    MYLIBsys="-L$LAPACK_PATH -mkl=sequential"
    NETCDF_PATH=""

    ;;
esac

if test "$MPIFC" = "$FC"; then
   FC_OUTPUT="\$(SFC)"
   F90_OUTPUT="\$(SF90)"
   CC_OUTPUT="\$(SCC)"
else
   FC_OUTPUT="\$(DM_FC)"
   F90_OUTPUT="\$(DM_F90)"
   CC_OUTPUT="\$(DM_CC)"
fi

COREDIR=`pwd`
INC_DIR="$COREDIR/include"
INC_FLAGS="-I$INC_DIR -I$NETCDF/include" # may need some work
FFLAGS_ARCH="$FFLAGS_DEFAULT $optimization_FFLAGS $world_size_FFLAGS"
CPP_F90FLAGS="-traditional-cpp"
FFLAGS_OPT="-O3"
CFLAGS_ARCH="-O0 -DLINUX -DUNDERSCORE"
CFLAGS2="-DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'"

####
# Below for the purpose of building libs and main via autotools
####

# For bacio lib
CFLAGS_BACIO="$CFLAGS2"
FFLAGS_BACIO="-O3 $FFLAGS_DEFAULT" 
ARFLAGS_BACIO=
AC_SUBST(CFLAGS_BACIO)
AC_SUBST(FFLAGS_BACIO)

# For burf lib
CPPFFLAGS_BUFR="$CPP_F90FLAGS"
CFLAGS_BUFR="-O3 -DUNDERSCORE"
FFLAGS_BUFR="-O3 $FFLAGS_DEFAULT $I4R8"
ARFLAGS_BUFR=
AC_SUBST(CPPFFLAGS_BUFR)
AC_SUBST(CFLAGS_BUFR)
AC_SUBST(FFLAGS_BUFR)

# For gsdcloud lib
FFLAGS_CLOUD="-O3 $FFLAGS_DEFAULT"
AC_SUBST(FFLAGS_CLOUD)

# For crtm lib
FFLAGS_CRTM="$CRTM_OPT $FFLAGS_DEFAULT"
AC_SUBST(FFLAGS_CRTM)

# For nemsio lib
libnemsio_a_FC="$DM_F90"
AC_SUBST(libnemsio_a_FC)

# For gfsio lib !!! not used !!!
FFLAGS_GFSIO="-O3 $FFLAGS_DEFAULT $I4R4"
ARFLAGS_GFSIO=

# For sfcio lib
FFLAGS_SFCIO="-O3 $FFLAGS_DEFAULT $I4R4"
AC_SUBST(FFLAGS_SFCIO)

# For sigio lib
FFLAGS_SIGIO="-O3 $FFLAGS_DEFAULT $I4R4"
AC_SUBST(FFLAGS_SIGIO)
ARFLAGS_SIGIO=

# For sp lib
CPPFLAGS_SP="-C -DLINUX"
FFLAGS_SP="-O3 $FFLAGS_DEFAULT $I4R8"
AC_SUBST(CPPFLAGS_SP)
AC_SUBST(FFLAGS_SP)

# For w3emc_v2.0.5 lib
FFLAGS_W3="-O3 $FFLAGS_DEFAULT"
AC_SUBST(FFLAGS_W3)

# For w3nco_v2.0.6 lib
CFLAGS_W3="$CFLAGS_ARCH"
AC_SUBST(CFLAGS_W3)

# For gsi.exe
CFLAGS_GSI="$CFLAGS2"
CPP_FFLAGS=`echo $CPP_FLAGS | sed -e 's/-P//' -e 's/-C//'`
FCFLAGS_GSI="$CPP_FFLAGS $FFLAGS_ARCH"
LDFLAGS_GSI="$LDFLAGS_ARCH"
LIBpackage="-L$CORELIB -lbacio -lbufr_i4r8 -lcrtm -lgsdcloud -lnemsio -lprepdecode_i4r8 -lsfcio_i4r4 -lsigio_i4r4 -lsp_i4r8 -lw3emc_i4r8 -lw3nco_i4r8"
LIBwrf="$WRF_DIR/frame/module_internal_header_util.o $WRF_DIR/frame/pack_utils.o $WRF_DIR/frame/module_machine.o $WRF_DIR/main/libwrflib.a -L$WRF_DIR/external/io_int -lwrfio_int -L$WRF_DIR/external/io_netcdf -lwrfio_nf"
LIBnetcdf="-L$NETCDF/lib -lnetcdff -lnetcdf"
LIBsys="$MYLIBsys"
gsi_exe_FC="$DM_F90"
gsi_exe_CC="$CC"

# Below for building gsi.exe using autotools on zeus and wcoss

if test "$hostname" = "zeus.rdhpcs.noaa.gov"; then
   # ----------------------------------
   # Redefine variables for NOAA HPCS
   # ----------------------------------
   COREINC=/contrib/nceplibs/nwprod/lib/incmod
   CORELIB=/contrib/nceplibs/nwprod/lib
   INCsfcio="$COREINC/sfcio_v1.1.0"
   INCsigio="$COREINC/sigio_4"
   INCnemsio="$COREINC/nemsio"
   INCw3="$COREINC/w3emc_d"
   INCcrtm="$COREINC/crtm_v2.1.3"

   WRFPATH=/scratch1/portfolios/NCEPDEV/da/save/Daryl.Kleist/nwprod/sorc/nam_nmm_real_fcst.fd
   LIBwrfio_int="$WRFPATH/external/io_int/libwrfio_int.a"
   LIBwrfio_netcdf="$WRFPATH/external/io_netcdf/libwrfio_nf.a"
   OBJwrf_frame_pk="$WRFPATH/frame/pack_utils.o"
   OBJwrf_sizeof_int="$WRFPATH/frame/module_machine.o"
   WRFLIB="$LIBwrfio_int $LIBwrfio_netcdf $OBJwrf_frame_pk $OBJwrf_sizeof_int"

   INCnetcdf="${NETCDF}/include"

#  LIBS="-L$CORELIB -lsp_d -lnemsio -lbacio_4 -lsigio_4 \
#  -lsfcio -lbufr_d_64 -lw3nco_d -lw3emc_d -lcrtm_v2.1.3 \
#  $WRFLIB -L$NETCDF/lib -lnetcdf

   LIBpackage="-L$CORELIB -lsp_d -lnemsio -lbacio_4 -lsigio_4 -lsfcio -lbufr_d_64 -lw3nco_d -lw3emc_d -lcrtm_v2.1.3"
   LIBwrf="$LIBwrfio_int $LIBwrfio_netcdf $OBJwrf_frame_pk $OBJwrf_sizeof_int"
   LIBnetcdf="-L$NETCDF/lib -lnetcdf"
   LIBsys=""
   MPI_INCLUDE=""
   INC_FLAGS=""

   CF="ifort"
   FC="$CF"
   CC="gcc"

   gsi_exe_FC="$FC"
   gsi_exe_CC="$CC"

   PROF=""
   OMP=""

#  libswap_CFLAGS="-DLINUX -Dfunder -I ./ \
#  -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -O3 $PROF"

   CFLAGS_GSI="-DLINUX -Dfunder -I ./ \
   -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -O3 $PROF"

   FFLAGS_F90="-D_REAL8_ -DWRF -openmp"

   FFLAGS_COM="-I ./ -I $INCcrtm -I $INCsfcio -I $INCsigio \
   -I $INCnemsio -I $INCnetcdf -I $INCw3 \
   -O3 -traceback -convert big_endian -assume byterecl \
   -g -fp-model strict"

   FFLAGS_COM_NOSWAP="-I ./ -I $INCcrtm -I $INCsfcio -I $INCsigio \
   -I $INCnemsio -I $INCnetcdf -I $INCw3 \
   -O3 -traceback -g -fp-model strict \
   -convert big_endian -assume byterecl"

#  gsi_exe_LDFLAGS="-mkl -lmpi -Wl,-Map,loadmap.txt"

   LDFLAGS_GSI="-mkl -lmpi -Wl,-Map,loadmap.txt"

#  libswap_FFLAGS="$FFLAGS_F90 $FFLAGS_COM $PROF"
#  libnoswap_FFLAGS="$FFLAGS_F90 $FFLAGS_COM_NOSWAP $PROF"

   FCFLAGS_GSI="$FFLAGS_F90 $FFLAGS_COM $PROF"

   # debug mode (use --enable-debug) for zeus

   if test "$enable_debug" = "yes"; then
#     libswap_CFLAGS="-DLINUX -Dfunder -I ./ \
#     -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -g"

      CFLAGS_GSI="-DLINUX -Dfunder -I ./ \
      -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -g"

      FFLAGS_COM="-I ./ -I $INCcrtm -I $INCsfcio -I $INCsigio \
      -I $INCnemsio -I $INCnetcdf -I $INCw3 \
      -O0 -fp-model strict -convert big_endian -assume byterecl \
      -implicitnone -g -traceback -debug \
      -ftrapuv -check all  -fp-stack-check  -fstack-protector -warn"

      FFLAGS_COM_NOSWAP="-I ./ -I $INCcrtm -I $INCsfcio -I $INCsigio \
      -I $INCnemsio -I $INCnetcdf -I $INCw3 \
      -O0 -fp-model strict -convert big_endian -assume byterecl \
      -implicitnone -g -traceback -debug \
      -ftrapuv -check all  -fp-stack-check  -fstack-protector -warn"

#     libswap_FFLAGS="$FFLAGS_F90 $FFLAGS_COM"
#     libnoswap_FFLAGS="$FFLAGS_F90 $FFLAGS_COM_NOSWAP"

      FCFLAGS_GSI="$FFLAGS_F90 $FFLAGS_COM"
   fi

elif test "${hostname}" = "wcoss.rdhpcs.noaa.gov"; then
   # ----------------------------------
   # Redefine variables for WCOSS
   # ----------------------------------

   # Set library versions
   BACIO_VERSION=2.0.1
   BUFR_VERSION=10.2.5
   CRTM_VERSION=2.1.3
   NEMSIO_VERSION=2.2.1
   NETCDF_VERSION=3.6.3
   SFCIO_VERSION=1.0.0
   SIGIO_VERSION=1.0.1
   SP_VERSION=2.0.1
   W3EMC_VERSION=2.0.5
   W3NCO_VERSION=2.0.6

   COREINC=/nwprod/lib/incmod
   CORELIB=/nwprod/lib
   INCsfcio="$COREINC/sfcio_v${SFCIO_VERSION}_4"
   INCsigio="$COREINC/sigio_v${SIGIO_VERSION}_4"
   INCnemsio="$COREINC/nemsio_v${NEMSIO_VERSION}"
   INCw3="$COREINC/w3emc_v${W3EMC_VERSION}_d"

   CRTM_PATH=/usrx/local/nceplibs
   INCcrtm="$CRTM_PATH/incmod/crtm_v$CRTM_VERSION"


   # WRF locations
   WRFPATH=/nwprod/sorc/wrf_shared.fd
   LIBwrfio_int="$WRFPATH/external/io_int/libwrfio_int.a"
   LIBwrfio_netcdf="$WRFPATH/external/io_netcdf/libwrfio_nf.a"
   OBJwrf_frame_pk="$WRFPATH/frame/pack_utils.o"
   OBJwrf_sizeof_int="$WRFPATH/frame/module_machine.o"
   WRFLIB="$LIBwrfio_int $LIBwrfio_netcdf $OBJwrf_frame_pk $OBJwrf_sizeof_int"

   NETCDFPATH="/usrx/local/NetCDF/$NETCDF_VERSION"
   INCnetcdf="$NETCDFPATH/include"

#  LIBS="-L$CORELIB -lsp_v${SP_VERSION}_d -lnemsio_v${NEMSIO_VERSION} \
#  -lbacio_v${BACIO_VERSION}_4 -lsigio_v${SIGIO_VERSION}_4 \
#  -lsfcio_v${SFCIO_VERSION}_4 -lbufr_v${BUFR_VERSION}_d_64 \
#  -lw3nco_v${W3NCO_VERSION}_d -lw3emc_v${W3EMC_VERSION}_d \
#  $WRFLIB -L$CRTM_PATH -lcrtm_v${CRTM_VERSION} \
#  -L$NETCDFPATH/lib -lnetcdf"

   LIBpackage="-L$CORELIB -lsp_v${SP_VERSION}_d -lnemsio_v${NEMSIO_VERSION} \
   -lbacio_v${BACIO_VERSION}_4 -lsigio_v${SIGIO_VERSION}_4 \
   -lsfcio_v${SFCIO_VERSION}_4 -lbufr_v${BUFR_VERSION}_d_64 \
   -lw3nco_v${W3NCO_VERSION}_d -lw3emc_v${W3EMC_VERSION}_d \
   -L$CRTM_PATH -lcrtm_v${CRTM_VERSION}"
   LIBwrf="$LIBwrfio_int $LIBwrfio_netcdf $OBJwrf_frame_pk $OBJwrf_sizeof_int"
   LIBnetcdf="-L$NETCDFPATH/lib -lnetcdf"
   LIBsys=""
   MPI_INCLUDE=""
   INC_FLAGS=""

   CF="mpfort"
   FC="$CF"
   CC="mpcc"

   gsi_exe_FC="$FC"
   gsi_exe_CC="$CC"

   OMP="-openmp"

#  libswap_CFLAGS="-DLINUX -Dfunder -I ./ \
#  -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -O3 $OMP"

   CFLAGS_GSI="-DLINUX -Dfunder -I ./ \
   -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -O3 $OMP"

   FFLAGS_F90="-D_REAL8_ -DWRF"

   FFLAGS_COM="-I ./ -I $INCcrtm -I $INCsfcio -I $INCsigio \
   -I $INCnemsio -I $INCnetcdf -I $INCw3 \
   -O3 -fp-model strict -convert big_endian -assume byterecl \
   -implicitnone $OMP"

   FFLAGS_COM_NOSWAP="-I ./ -I $INCcrtm -I $INCsfcio -I $INCsigio \
   -I $INCnemsio -I $INCnetcdf -I $INCw3 \
   -O3 -fp-model strict -convert big_endian -assume byterecl \
   -implicitnone $OMP"

#  gsi_exe_LDFLAGS="-mkl -Wl,-Map,loadmap.txt $OMP"

   LDFLAGS_GSI="-mkl -Wl,-Map,loadmap.txt $OMP"

#  libswap_FFLAGS="$FFLAGS_F90 $FFLAGS_COM $PROF"
#  libnoswap_FFLAGS="$FFLAGS_F90 $FFLAGS_COM_NOSWAP $PROF"

   FCFLAGS_GSI="$FFLAGS_F90 $FFLAGS_COM $PROF"

   # debug mode (use --enable-debug) for wcoss

   if test "$enable_debug" = "yes"; then
#    libswap_CFLAGS="-DLINUX -Dfunder -I ./ \
#    -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -g"

     CFLAGS_GSI="-DLINUX -Dfunder -I ./ \
     -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -g"

     FFLAGS_COM="-I ./ -I $INCcrtm -I $INCsfcio -I $INCsigio \
     -I $INCnemsio -I $INCnetcdf -I $INCw3 \
     -O0 -fp-model strict -convert big_endian -assume byterecl \
     -implicitnone -g -traceback -debug \
     -ftrapuv -check all  -fp-stack-check  -fstack-protector -warn"

     FFLAGS_COM_NOSWAP="-I ./ -I $INCcrtm -I $INCsfcio -I $INCsigio \
     -I $INCnemsio -I $INCnetcdf -I $INCw3 \
     -O0 -fp-model strict -convert big_endian -assume byterecl \
     -implicitnone -g -traceback -debug \
     -ftrapuv -check all  -fp-stack-check  -fstack-protector -warn"

#    gsi_exe_LDFLAGS="-mkl -Wl,-Map,loadmap.txt"

     LDFLAGS_GSI="-mkl -Wl,-Map,loadmap.txt"

#    libswap_FFLAGS="$FFLAGS_F90 $FFLAGS_COM"
#    libnoswap_FFLAGS="$FFLAGS_F90 $FFLAGS_COM_NOSWAP"

     FCFLAGS_GSI="$FFLAGS_F90 $FFLAGS_COM"
   fi
fi

# Above for building gsi.exe using autotools on zeus and wcoss

# Passing required variables into src/main makefile

AC_SUBST(CFLAGS_GSI)
AC_SUBST(FCFLAGS_GSI)
AC_SUBST(LDFLAGS_GSI)
AC_SUBST(LIBpackage)
AC_SUBST(LIBwrf)
AC_SUBST(LIBnetcdf)
AC_SUBST(LIBsys)
AC_SUBST(MPI_INCLUDE)
AC_SUBST(INC_FLAGS)
AC_SUBST(gsi_exe_FC)
AC_SUBST(gsi_exe_CC)
AC_OUTPUT

####
# Output configure.gsi
####

cat << END_CONFIG > configure.gsi
SHELL          		=       /bin/sh

LDFLAGS        =  $LDFLAGS_ARCH

COREDIR        =  $COREDIR
INC_DIR        =  \$(COREDIR)/include
SFC            =  $FC
SF90           =  $SF90
SCC            =  $SCC
INC_FLAGS      =  -module \$(INC_DIR) -I \$(INC_DIR) -I $NETCDF/include
FFLAGS_i4r4    =  $I4R4
FFLAGS_i4r8    =  $I4R8
FFLAGS_i8r8    =  $I8R8
FFLAGS_DEFAULT =  $FFLAGS_DEFAULT
FFLAGS_DEBUG   =  $FFLAGS_DEBUG
FFLAGS_OPT     =  $FFLAGS_OPT
FFLAGS         =  $FFLAGS_OUTPUT
# Library build flags
FFLAGS_BACIO   =  $FFLAGS_BACIO
ARFLAGS_BACIO  =  
FFLAGS_BUFR    =  $FFLAGS_BUFR
CFLAGS_BUFR    =  $CFLAGS_BUFR
ARFLAGS_BUFR   =  $ARFLAGS_BUFR
FFLAGS_CLOUD   =  $FFLAGS_CLOUD
FFLAGS_CRTM    =  $FFLAGS_CRTM
FFLAGS_GFSIO   =  $FFLAGS_GFSIO
ARFLAGS_GFSIO  =  $ARFLAGS_GFSIO
FFLAGS_SFCIO   =  $FFLAGS_SFCIO
FFLAGS_SIGIO   =  $FFLAGS_SIGIO
ARFLAGS_SIGIO  =  $ARFLAGS_SIGIO
FFLAGS_SP      =  $FFLAGS_SP
FFLAGS_W3      =  $FFLAGS_W3
# End of library build flags
#
CPP            =  $CPP
CPP_FLAGS      =  $CPP_FLAGS
CPP_F90FLAGS   =  $CPP_F90FLAGS

DM_FC          =  $DM_FC
DM_F90         =  $DM_F90
DM_CC          =  $DM_CC

FC             =  $FC_OUTPUT
F90            =  $F90_OUTPUT
CC             =  $CC_OUTPUT

CFLAGS         =  $CFLAGS_ARCH
CFLAGS2        =  $CFLAGS2
MYLIBsys       =  $MYLIBsys

NETCDF_PATH    =  $NETCDF_PATH

###########################################################
#
#       Macros, these should be generic for all machines

MAKE            =  make -i -r
RM              =  /bin/rm -f
CP              =  /bin/cp
AR              =  ar 
MKDIR           =  /bin/mkdir -p

WRF_INCLUDE     =  -I$WRF_DIR/frame

WRF_LIB         =    \$(WRF_DIR)/frame/module_internal_header_util.o \\
                     \$(WRF_DIR)/frame/pack_utils.o \\
                     \$(WRF_DIR)/frame/module_machine.o \\
                     \$(WRF_DIR)/main/libwrflib.a \\
                     -L\$(WRF_DIR)/external/io_int -lwrfio_int \\
                     -L\$(WRF_DIR)/external/io_netcdf -lwrfio_nf


WRF_LIB2        =  -L\$(WRF_DIR)/external/io_grib2 -lio_grib2 -L/usr/lib64 -ljasper
NETCDFPATH      =  $NETCDF
NETCDFLIBS      =  -lnetcdff -lnetcdf
WRF_DIR         =  $WRF_DIR



.IGNORE:
.SUFFIXES: .c .f .F .F90 .f90 .o .fpp

#       There is probably no reason to modify these rules

.c.o:
	\$(RM) \$@
	\$(CC) \$(CFLAGS2) -c \$<
.f.o:
	\$(RM) \$@ \$*.mod
	\$(FC) \$(FFLAGS) -c \$< 

.fpp.o:
	\$(RM) \$@ \$*.mod \$*.f
	\$(CPP) \$(CPP_FLAGS) \$< > \$*.f
	\$(FC) \$(FFLAGS) -c \$*.f 
	\$(RM) \$*.f

.F.o:
	\$(RM) \$@ \$*.mod
	\$(CPP) \$(CPP_FLAGS) \$< > \$*.f90
	\$(F90) \$(FFLAGS) -c \$*.f90 
	\$(RM) \$*.f90

.F90.o:
	\$(RM) \$@ \$*.mod
	\$(CPP) \$(CPP_FLAGS) \$< > \$*.f90
	\$(F90) \$(FFLAGS) -c \$*.f90 
	\$(RM) \$*.f90

.f90.o:
	\$(RM) \$@ \$*.mod
	\$(F90) \$(FFLAGS) -c $< 
END_CONFIG

# Below outputing Makefile.conf on either zeus or wcoss

if test "$hostname" = "zeus.rdhpcs.noaa.gov"; then
cat << END_MAKEFILE_CONF > src/main/Makefile.conf
# This config file contains the compile options for compilation
# of the GSI code on the NOAA HPCS.

# ----------------------------------
# Redefine variables for NOAA HPCS
# ----------------------------------
COREINC  = /contrib/nceplibs/nwprod/lib/incmod
CORELIB  = /contrib/nceplibs/nwprod/lib
INCsfcio = \$(COREINC)/sfcio_v1.1.0
INCsigio = \$(COREINC)/sigio_4
INCnemsio= \$(COREINC)/nemsio
INCw3    = \$(COREINC)/w3emc_d
INCcrtm=\$(COREINC)/crtm_v2.1.3

WRFPATH = /scratch1/portfolios/NCEPDEV/da/save/Daryl.Kleist/nwprod/sorc/nam_nmm_real_fcst.fd
LIBwrfio_int    = \$(WRFPATH)/external/io_int/libwrfio_int.a
LIBwrfio_netcdf = \$(WRFPATH)/external/io_netcdf/libwrfio_nf.a
OBJwrf_frame_pk = \$(WRFPATH)/frame/pack_utils.o
OBJwrf_sizeof_int = \$(WRFPATH)/frame/module_machine.o
WRFLIB = \$(LIBwrfio_int) \$(LIBwrfio_netcdf) \$(OBJwrf_frame_pk) \$(OBJwrf_sizeof_int)

INCnetcdf       = \${NETCDF}/include
#WRFPATH =
#LIBwrfio_int    =
#LIBwrfio_netcdf =
#OBJwrf_frame_pk =
#OBJwrf_sizeof_int =
#WRFLIB =

# Empty out definition of libs use by GMAO GSI building structure
LIBtransf = #-L\$(CORELIB) -ltransf
LIBhermes = #-L\$(CORELIB) -lhermes
LIBgfio   = #-L\$(CORELIB) -lgfio
INChdf    = #-I\$(BASEDIR)/\$(ARCH)/include/hdf
LIBhdf    = #-L\$(BASEDIR)/\$(ARCH)/lib  -lmfhdf -ldf -lhdfjpeg -lhdfz -lsz


# ---------------------------
# General targets/definitions
# ---------------------------

  AR     = ar -v

# ----------------------------
# Fortran compiler and options
# ----------------------------

  CF            = ifort
  FC            = \$(CF)

#--- Normal mode options
  PROF= #-pg
  OMP = #-qsmp=noauto

  FFLAGS_F90    = -D_REAL8_ -DWRF -openmp

  FFLAGS_COM_N  = -I ./ -I \$(INCcrtm) -I \$(INCsfcio) -I \$(INCsigio) -I \$(INCnemsio) \\
                -I \$(INCnetcdf) -I \$(INCw3) -O3 -traceback -convert big_endian -assume byterecl \\
	        -g -fp-model strict

  FFLAGS_COM_N_NOSWAP  = -I ./ -I \$(INCcrtm) -I \$(INCsfcio) -I \$(INCsigio) \\
                  -I \$(INCnemsio) -I \$(INCnetcdf) -I \$(INCw3) \\
		  -O3 -traceback -g -fp-model strict \\
		  -convert big_endian -assume byterecl

  FFLAGS_N  = \$(FFLAGS_F90) \$(FFLAGS_COM_N) \$(PROF)
  FFLAGS_NOSWAP_N  = \$(FFLAGS_F90) \$(FFLAGS_COM_N_NOSWAP) \$(PROF)
  FFLAGS_f  = \$(FFLAGS_COM_N) \$(PROF)
  FFLAGS    = \$(FFLAGS_N) \$(PROF)


#--- Debug mode options
  FFLAGS_COM_D  =  -I ./ -I \$(INCcrtm) -I \$(INCsfcio) -I \$(INCsigio) -I \$(INCnemsio) \\
		-I \$(INCnetcdf) -I \$(INCw3) \\
		-O0 -fp-model strict -convert big_endian -assume byterecl \\
		-implicitnone -g -traceback -debug \\
		-ftrapuv -check all  -fp-stack-check  -fstack-protector -warn
  FFLAGS_COM_NOSWAP_D  =  -I ./ -I \$(INCcrtm) -I \$(INCsfcio) -I \$(INCsigio) \\
		-I \$(INCnemsio) -I \$(INCnetcdf) -I \$(INCw3) \\
		-O0 -fp-model strict -convert big_endian -assume byterecl \\
		-implicitnone -g -traceback -debug \\
		-ftrapuv -check all  -fp-stack-check  -fstack-protector -warn
  
  FFLAGS_D  = \$(FFLAGS_F90) \$(FFLAGS_COM_D)
  FFLAGS_NOSWAP_D = \$(FFLAGS_F90) \$(FFLAGS_COM_NOSWAP_D)



# ----------------------
# C Compiler and options
# ----------------------

  CC            = gcc

#--- Normal mode options

  CFLAGS_N      = -DLINUX -Dfunder -I ./ -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -O3 \$(PROF)

#--- Debug mode options

  CFLAGS_D      = -DLINUX -Dfunder -I ./ -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -g


# ------------------
# Linker and options
# ------------------

  LD            = \$(CF) -mkl -lmpi -Wl,-Map,loadmap.txt

#--- Normal mode options
# LIBS_N        = -L\$(CORELIB) -lsp_d -lnemsio -lbacio_4 -lsigio_4 \\
#      -lsfcio_4 -lbufr_d_64 -lw3lib-2.0_d \$(LIBcrtm) \$(WRFLIB) -L\$(NETCDF)/lib -lnetcdf
  LIBS_N        = -L\$(CORELIB) -lsp_d -lnemsio -lbacio_4 -lsigio_4 \\
       -lsfcio -lbufr_d_64 -lw3nco_d -lw3emc_d -lcrtm_v2.1.3 \$(WRFLIB) -L\$(NETCDF)/lib -lnetcdf
#      -L../lib/GSD/gsdcloud4nmmb -lgsdcloud
# LIBS_N        = \$(LIBnemsio) -L\$(CORELIB) -lsp_d -lsigio_4 \\
#      -lsfcio_4 -lbufr_dc -lw3lib-2.0_d \$(LIBcrtm) \$(WRFLIB) \$(LIBbacio) -L\$(NETCDF)/lib -lnetcdf

  LDFLAGS_N     = 

  LDFLAGS       = \$(LDFLAGS_N)

#--- Debug mode options

  LIBS_D        = \$(LIBS_N)

  LDFLAGS_D     =

#--- Empty out mpi library definition: embedded in compiler

  LIBmpi	= 
END_MAKEFILE_CONF

elif test "\\$hostname" = "wcoss.rdhpcs.noaa.gov"; then
cat << END_MAKEFILE_CONF > src/main/Makefile.conf
# This config file contains the compile options for compilation
# of the GSI code on the NOAA HPCS.

# ----------------------------------
# Redefine variables for WCOSS
# ----------------------------------

# Set library versions
BACIO_VERSION = 2.0.1
BUFR_VERSION = 10.2.5
CRTM_VERSION = 2.1.3
NEMSIO_VERSION = 2.2.1
NETCDF_VERSION = 3.6.3
SFCIO_VERSION = 1.0.0
SIGIO_VERSION = 1.0.1
SP_VERSION = 2.0.1
W3EMC_VERSION = 2.0.5
W3NCO_VERSION = 2.0.6

COREINC  = /nwprod/lib/incmod
CORELIB  = /nwprod/lib
INCsfcio = \$(COREINC)/sfcio_v\$(SFCIO_VERSION)_4
INCsigio = \$(COREINC)/sigio_v\$(SIGIO_VERSION)_4
INCnemsio= \$(COREINC)/nemsio_v\$(NEMSIO_VERSION)
INCw3    = \$(COREINC)/w3emc_v\$(W3EMC_VERSION)_d


CRTM_PATH    = /usrx/local/nceplibs
INCcrtm      = \$(CRTM_PATH)/incmod/crtm_v\$(CRTM_VERSION)


# WRF locations
WRFPATH = /nwprod/sorc/wrf_shared.fd
LIBwrfio_int    = \$(WRFPATH)/external/io_int/libwrfio_int.a
LIBwrfio_netcdf = \$(WRFPATH)/external/io_netcdf/libwrfio_nf.a
OBJwrf_frame_pk = \$(WRFPATH)/frame/pack_utils.o
OBJwrf_sizeof_int = \$(WRFPATH)/frame/module_machine.o
WRFLIB = \$(LIBwrfio_int) \$(LIBwrfio_netcdf) \$(OBJwrf_frame_pk) \$(OBJwrf_sizeof_int)

NETCDFPATH = /usrx/local/NetCDF/\$(NETCDF_VERSION)
INCnetcdf  = \$(NETCDFPATH)/include



# Empty out definition of libs use by GMAO GSI building structure
LIBtransf = #-L\$(CORELIB) -ltransf
LIBhermes = #-L\$(CORELIB) -lhermes
LIBgfio   = #-L\$(CORELIB) -lgfio
INChdf    = #-I\$(BASEDIR)/\$(ARCH)/include/hdf
LIBhdf    = #-L\$(BASEDIR)/\$(ARCH)/lib  -lmfhdf -ldf -lhdfjpeg -lhdfz -lsz


# ---------------------------
# General targets/definitions
# ---------------------------

  AR     = ar -v

# ----------------------------
# Fortran compiler and options
# ----------------------------

  CF            = mpfort
  FC            = \$(CF)

#--- Normal mode options
  PROF= #-pg
  OMP = -openmp

  FFLAGS_F90    = -D_REAL8_ -DWRF

  FFLAGS_COM_N  = -I ./ -I \$(INCcrtm) -I \$(INCsfcio) -I \$(INCsigio) -I \$(INCnemsio) \\
  		-I \$(INCnetcdf) -I \$(INCw3) \\
                -O3 -fp-model strict -convert big_endian -assume byterecl \\
                -implicitnone \$(OMP)

  FFLAGS_COM_N_NOSWAP  = -I ./ -I \$(INCcrtm) -I \$(INCsfcio) -I \$(INCsigio) -I \$(INCnemsio) \\
  		       -I \$(INCnetcdf) -I \$(INCw3) \\
                       -O3 -fp-model strict -convert big_endian -assume byterecl \\
                       -implicitnone \$(OMP)

  FFLAGS_N  = \$(FFLAGS_F90) \$(FFLAGS_COM_N) \$(PROF)
  FFLAGS_NOSWAP_N  = \$(FFLAGS_F90) \$(FFLAGS_COM_N_NOSWAP) \$(PROF)
  FFLAGS_f  = \$(FFLAGS_COM_N) \$(PROF)
  FFLAGS    = \$(FFLAGS_N) \$(PROF)


#--- Debug mode options
  FFLAGS_COM_D  =  -I ./ -I \$(INCcrtm) -I \$(INCsfcio) -I \$(INCsigio) -I \$(INCnemsio) \\
  		-I \$(INCnetcdf) -I \$(INCw3) \\
  		-O0 -fp-model strict -convert big_endian -assume byterecl \\
		-implicitnone -g -traceback -debug \\
                -ftrapuv -check all  -fp-stack-check  -fstack-protector -warn
  FFLAGS_COM_NOSWAP_D  =  -I ./ -I \$(INCcrtm) -I \$(INCsfcio) -I \$(INCsigio) -I \$(INCnemsio) \\
  		       -I \$(INCnetcdf) -I \$(INCw3) \\
  		       -O0 -fp-model strict -convert big_endian -assume byterecl \\
		       -implicitnone -g -traceback -debug \\
                       -ftrapuv -check all  -fp-stack-check  -fstack-protector -warn
  FFLAGS_D  = \$(FFLAGS_F90) \$(FFLAGS_COM_D)
  FFLAGS_NOSWAP_D = \$(FFLAGS_F90) \$(FFLAGS_COM_NOSWAP_D)



# ----------------------
# C Compiler and options
# ----------------------

  CC            = mpcc

#--- Normal mode options

  CFLAGS_N      = -DLINUX -Dfunder -I ./ -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -O3 \$(OMP)

#--- Debug mode options

  CFLAGS_D      = -DLINUX -Dfunder -I ./ -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -g


# ------------------
# Linker and options
# ------------------

  LD            = \$(CF)

#--- Normal mode options

  LIBS_N        = -L\$(CORELIB) -lsp_v\$(SP_VERSION)_d -lnemsio_v\$(NEMSIO_VERSION) \\
  		-lbacio_v\$(BACIO_VERSION)_4 -lsigio_v\$(SIGIO_VERSION)_4 \\
		-lsfcio_v\$(SFCIO_VERSION)_4 -lbufr_v\$(BUFR_VERSION)_d_64 \\
		-lw3nco_v\$(W3NCO_VERSION)_d -lw3emc_v\$(W3EMC_VERSION)_d \\
		\$(WRFLIB) -L\$(CRTM_PATH) -lcrtm_v\$(CRTM_VERSION) \\
		-L\$(NETCDFPATH)/lib -lnetcdf
#               -L../lib/GSD/gsdcloud4nmmb -lgsdcloud

  LDFLAGS_N     = -mkl -Wl,-Map,loadmap.txt \$(OMP)

  LDFLAGS       = \$(LDFLAGS_N)

#--- Debug mode options

  LIBS_D        = \$(LIBS_N)

  LDFLAGS_D     = -mkl -Wl,-Map,loadmap.txt

#--- Empty out mpi library definition: embedded in compiler

  LIBmpi	= 
END_MAKEFILE_CONF
fi

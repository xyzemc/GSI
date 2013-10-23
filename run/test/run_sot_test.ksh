#!/bin/ksh 
#####################################################
# machine set up (users should change this part)
#####################################################

#
# GSIPROC = processor number used for GSI analysis
#------------------------------------------------
# GSIPROC=1,2,4
  if [ $GSIPROC = 1 ] ; then
     TEST_DIR_SUFFIX='np1'
  elif [ $GSIPROC = 2 ] ; then
     TEST_DIR_SUFFIX='np2'
  elif [ $GSIPROC = 4 ] ; then
     TEST_DIR_SUFFIX='np4'
  else
     echo "ERROR: GSIPROC $GSIPRO is not = 1,2, or 4."
     exit 1
  fi 

ARCH='LINUX'
# Supported configurations:
            # IBM_LSF,IBM_LoadLevel
            # LINUX, LINUX_LSF, LINUX_PBS,
            # DARWIN_PGI
#
#####################################################
# case set up (users should change this part)
#####################################################
# bk_core= which WRF core is used as background (NMM or ARW)
# bkcv_option= which background error covariance and parameter will be used 
#              (GLOBAL or NAM)
# bk_core=ARW
  if [ ${bk_core} = NMM ] ; then
    CORE_DIR=nmm
  else
    CORE_DIR=arw
  fi
  bkcv_option=NAM

#------------------------------------------------
#
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working directory, where GSI runs
# PREPBURF = path of PreBUFR conventional obs
# BK_FILE  = path and name of background file
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# GSI_EXE  = path and name of the gsi executable 
  ANAL_TIME=2011060212
# COMPILER=pgi
  GSI_TEST=sot
# GSI_TEST=con
# GSI_TEST=rad
# GSI_TEST=gpsro
# GSI_TEST=radar
# GSI_ROOT=/d1/stark/GSI
  GSI_DIR=${COMPILER}/${GSI_TEST_DIR}
  WORK_ROOT=${GSI_ROOT}/run/${GSI_DIR}/${GSI_TEST}_${ANAL_TIME}_${CORE_DIR}_${TEST_DIR_SUFFIX}
  DATA_DIR=${GSI_ROOT}/data
#
  BK_FILE=${DATA_DIR}/${ANAL_TIME}/${CORE_DIR}/wrfinput_d01_2011-06-02_12:00:00
  OBS_ROOT=${DATA_DIR}/${ANAL_TIME}/obs
  PREPBUFR=${OBS_ROOT}/nam.t12z.prepbufr.tm00.nr.le
  CRTM_ROOT=${DATA_DIR}/fix/CRTM_Coefficients-2.0.5
  FIX_ROOT=${GSI_ROOT}/src/${GSI_DIR}/fix
  GSI_EXE=${GSI_ROOT}/src/${GSI_DIR}/run/gsi.exe

# if_clean = clean  : delete temperal files in working directory (default)
#            no     : leave running directory as is (this is for debug only)
  if_clean=clean
#
#------------------------------------------------
# GSI hybrid options
# if_hybrid = .true. then turn on hybrid data analysis 
# ntotmem = number of ensemble members used for hybrid
# mempath = path of ensemble members
# 
# please note that we assume the ensemble member are located
#          under ${mempath} with name wrfout_d01_${iiimem}.
#          If this is not the case, please change the following lines:
#
#      if [ -r  ${mempath}/wrfout_d01_${iiimem} ]; then
#         ln -sf ${mempath}/wrfout_d01_${iiimem} ./wrf_en${iiimem}
#      else
#         echo "member ${mempath}/wrfout_d01_${iiimem} is not exit"
#      fi
#
# 
  if_hybrid=.false.
  ntotmem=40
  mempath=/ensemble/wrfprd
#
#####################################################
# Users should NOT change script after this point
#####################################################
#
case $ARCH in
   'IBM_LSF')
      ###### IBM LSF (Load Sharing Facility)
      BYTE_ORDER=Big_Endian
      RUN_COMMAND="mpirun.lsf " ;;

   'IBM_LoadLevel')
      ###### IBM LoadLeve 
      BYTE_ORDER=Big_Endian
      RUN_COMMAND="poe " ;;

   'LINUX')
      BYTE_ORDER=Little_Endian
      if [ $GSIPROC = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
        RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile /home/stark/mach "
      fi ;;

   'LINUX_LSF')
      ###### LINUX LSF (Load Sharing Facility)
      BYTE_ORDER=Little_Endian
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_PBS')
      BYTE_ORDER=Little_Endian
      #### Linux cluster PBS (Portable Batch System)
      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;

   'DARWIN_PGI')
      ### Mac - mpi run
      BYTE_ORDER=Little_Endian
      if [ $GSIPROC = 1 ]; then
         #### Mac workstation - single processor
         RUN_COMMAND=""
      else
         ###### Mac workstation -  mpi run
         RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   * )
     print "error: $ARCH is not a supported platform configuration."
     exit 1 ;;
esac


##################################################################################
# Check GSI needed environment variables are defined and exist
#
 
# Make sure ANAL_TIME is defined and in the correct format
if [ ! "${ANAL_TIME}" ]; then
  echo "ERROR: \$ANAL_TIME is not defined!"
  exit 1
fi

# Make sure WORK_ROOT is defined and exists
if [ ! "${WORK_ROOT}" ]; then
  echo "ERROR: \$WORK_ROOT is not defined!"
  exit 1
fi

# Make sure the background file exists
if [ ! -r "${BK_FILE}" ]; then
  echo "ERROR: ${BK_FILE} does not exist!"
  exit 1
fi

# Make sure OBS_ROOT is defined and exists
if [ ! "${OBS_ROOT}" ]; then
  echo "ERROR: \$OBS_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${OBS_ROOT}" ]; then
  echo "ERROR: OBS_ROOT directory '${OBS_ROOT}' does not exist!"
  exit 1
fi

# Set the path to the GSI static files
if [ ! "${FIX_ROOT}" ]; then
  echo "ERROR: \$FIX_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${FIX_ROOT}" ]; then
  echo "ERROR: fix directory '${FIX_ROOT}' does not exist!"
  exit 1
fi

# Set the path to the CRTM coefficients 
if [ ! "${CRTM_ROOT}" ]; then
  echo "ERROR: \$CRTM_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${CRTM_ROOT}" ]; then
  echo "ERROR: fix directory '${CRTM_ROOT}' does not exist!"
  exit 1
fi


# Make sure the GSI executable exists
if [ ! -x "${GSI_EXE}" ]; then
  echo "ERROR: ${GSI_EXE} does not exist!"
  exit 1
fi

# Check to make sure the number of processors for running GSI was specified
if [ -z "${GSIPROC}" ]; then
  echo "ERROR: The variable $GSIPROC must be set to contain the number of processors to run GSI"
  exit 1
fi

#
##################################################################################
# Create the ram work directory and cd into it

workdir=${WORK_ROOT}
echo " Create working directory:" ${workdir}

if [ -d "${workdir}" ]; then
  rm -rf ${workdir}
fi
mkdir -p ${workdir}
cd ${workdir}

#
##################################################################################

echo " Copy GSI executable, background file, and link observation bufr to working directory"

# Save a copy of the GSI executable in the workdir
cp ${GSI_EXE} gsi.exe

# Bring over background field (it's modified by GSI so we can't link to it)
cp ${BK_FILE} ./wrf_inout

# Link ensember members if use hybrid
if [ ${if_hybrid} = .true. ] ; then
   echo " link ensemble members to working directory"

   let imem=1

   while (( $imem <= $ntotmem )); do

      if [[ $imem -lt 10 ]]; then
         export iiimem=00${imem}
         export iimem=0${imem}
      else
         export iiimem=0${imem}
         export iimem=${imem}
      fi

      let imem="imem + 1"

      if [ -r  ${mempath}/wrfout_d01_${iiimem} ]; then
         ln -sf ${mempath}/wrfout_d01_${iiimem} ./wrf_en${iiimem}
      else
         echo "member ${mempath}/wrfout_d01_${iiimem} is not exit"
         exit 1
      fi

   done
fi

# Link to the prepbufr data
ln -s ${PREPBUFR} ./prepbufr

# Link to the radiance data
# ln -s ${OBS_ROOT}/le_gdas1.t12z.1bamua.tm00.bufr_d amsuabufr
# ln -s ${OBS_ROOT}/le_gdas1.t12z.1bamub.tm00.bufr_d amsubbufr
# ln -s ${OBS_ROOT}/le_gdas1.t12z.1bhrs3.tm00.bufr_d hirs3bufr
# ln -s ${OBS_ROOT}/le_gdas1.t12z.1bhrs4.tm00.bufr_d hirs4bufr
# ln -s ${OBS_ROOT}/le_gdas1.t12z.1bmhs.tm00.bufr_d mhsbufr
# ln -s ${OBS_ROOT}/le_gdas1.t12z.gpsro.tm00.bufr_d gpsrobufr

#
##################################################################################

echo " Copy fixed files and link CRTM coefficient files to working directory"

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

if [ ${bkcv_option} = GLOBAL ] ; then
  echo ' Use global background error covariance'
  if [ ${BYTE_ORDER} = Little_Endian ] ; then
    BERROR=${FIX_ROOT}/nam_glb_berror.f77.gcv_Little_Endian
  else
    BERROR=${FIX_ROOT}/nam_glb_berror.f77.gcv
  fi
  OBERROR=${FIX_ROOT}/prepobs_errtable.global
else
  echo ' Use NAM background error covariance'
  if [ ${BYTE_ORDER} = Little_Endian ] ; then
    BERROR=${FIX_ROOT}/nam_nmmstat_na.gcv_Little_Endian
  else
    BERROR=${FIX_ROOT}/nam_nmmstat_na.gcv
  fi
  OBERROR=${FIX_ROOT}/nam_errtable.r3dv
fi

if [ ${bk_core} = NMM ] ; then
  ANAVINFO=${FIX_ROOT}/anavinfo_ndas_netcdf
else
  ANAVINFO=${FIX_ROOT}/anavinfo_arw_netcdf
fi

SATANGL=${FIX_ROOT}/global_satangbias.txt
SATINFO=${FIX_ROOT}/global_satinfo.txt
CONVINFO=${FIX_ROOT}/global_convinfo.txt
OZINFO=${FIX_ROOT}/global_ozinfo.txt
PCPINFO=${FIX_ROOT}/global_pcpinfo.txt

RTMFIX=${CRTM_ROOT}
RTMEMIS=${RTMFIX}/EmisCoeff/${BYTE_ORDER}/EmisCoeff.bin
RTMAERO=${RTMFIX}/AerosolCoeff/${BYTE_ORDER}/AerosolCoeff.bin
RTMCLDS=${RTMFIX}/CloudCoeff/${BYTE_ORDER}/CloudCoeff.bin

#  copy Fixed fields to working directory
 cp $ANAVINFO anavinfo
 cp $BERROR   berror_stats
 cp $SATANGL  satbias_angle
 cp $SATINFO  satinfo
 cp $CONVINFO convinfo
 cp $OZINFO   ozinfo
 cp $PCPINFO  pcpinfo
 cp $OBERROR  errtable
#
## CRTM Spectral and Transmittance coefficients
 ln -s $RTMEMIS  EmisCoeff.bin
 ln -s $RTMAERO  AerosolCoeff.bin
 ln -s $RTMCLDS  CloudCoeff.bin
 nsatsen=`cat satinfo | wc -l`
 isatsen=1
 while [[ $isatsen -le $nsatsen ]]; do
    flag=`head -n $isatsen satinfo | tail -1 | cut -c1-1`
    if [[ "$flag" != "!" ]]; then
       satsen=`head -n $isatsen satinfo | tail -1 | cut -f 2 -d" "`
       spccoeff=${satsen}.SpcCoeff.bin
       if  [[ ! -s $spccoeff ]]; then
          ln -s $RTMFIX/SpcCoeff/${BYTE_ORDER}/$spccoeff $spccoeff
          ln -s $RTMFIX/TauCoeff/${BYTE_ORDER}/${satsen}.TauCoeff.bin ${satsen}.TauCoeff.bin
       fi
    fi
    isatsen=` expr $isatsen + 1 `
 done

# Only need this file for single obs test
 bufrtable=${FIX_ROOT}/prepobs_prep.bufrtable
 cp $bufrtable ./prepobs_prep.bufrtable

# for satellite bias correction
cp ${DATA_DIR}/fix/sample.satbias ./satbias_in

#
##################################################################################
# Set some parameters for use by the GSI executable and to build the namelist
echo " Build the namelist "

export JCAP=62
export LEVS=60
export JCAP_B=62
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}

if [ ${bkcv_option} = GLOBAL ] ; then
   vs_op='0.7,'
   hzscl_op='1.7,0.8,0.5,'
else
   vs_op='1.0,'
   hzscl_op='0.373,0.746,1.50,'
fi

if [ ${bk_core} = NMM ] ; then
   bk_core_arw='.false.'
   bk_core_nmm='.true.'
   ensbk_option=2
else
   bk_core_arw='.true.'
   bk_core_nmm='.false.'
   ensbk_option=3
fi

# Build the GSI namelist on-the-fly
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=10,niter(2)=10,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=77,iguess=-1,
   oneobtest=.true.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=${bk_core_nmm},wrf_mass_regional=${bk_core_arw},
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.true.,
 /
 &BKGERR
   vs=${vs_op}
   hzscl=${hzscl_op}
   bw=0.,fstat=.true.,
 /
 &ANBKGERR
   anisotropic=.false.,an_vs=1.0,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=4.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',         dsis(04)='pw',                  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='satwnd',    dtype(05)='uv',        dplat(05)=' ',         dsis(05)='uv',                  dval(05)=1.0,  dthin(05)=0,
   dfile(06)='prepbufr',  dtype(06)='uv',        dplat(06)=' ',         dsis(06)='uv',                  dval(06)=1.0,  dthin(06)=0,
   dfile(07)='prepbufr',  dtype(07)='spd',       dplat(07)=' ',         dsis(07)='spd',                 dval(07)=1.0,  dthin(07)=0,
   dfile(08)='prepbufr',  dtype(08)='dw',        dplat(08)=' ',         dsis(08)='dw',                  dval(08)=1.0,  dthin(08)=0,
   dfile(09)='radarbufr', dtype(09)='rw',        dplat(09)=' ',         dsis(09)='rw',                  dval(09)=1.0,  dthin(09)=0,
   dfile(10)='prepbufr',  dtype(10)='sst',       dplat(10)=' ',         dsis(10)='sst',                 dval(10)=1.0,  dthin(10)=0,
   dfile(11)='gpsrobufr', dtype(11)='gps_ref',   dplat(11)=' ',         dsis(11)='gps',                 dval(11)=1.0,  dthin(11)=0,
   dfile(12)='ssmirrbufr',dtype(12)='pcp_ssmi',  dplat(12)='dmsp',      dsis(12)='pcp_ssmi',            dval(12)=1.0,  dthin(12)=-1,
   dfile(13)='tmirrbufr', dtype(13)='pcp_tmi',   dplat(13)='trmm',      dsis(13)='pcp_tmi',             dval(13)=1.0,  dthin(13)=-1,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n16',       dsis(14)='sbuv8_n16',           dval(14)=1.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n17',       dsis(15)='sbuv8_n17',           dval(15)=1.0,  dthin(15)=0,
   dfile(16)='sbuvbufr',  dtype(16)='sbuv2',     dplat(16)='n18',       dsis(16)='sbuv8_n18',           dval(16)=1.0,  dthin(16)=0,
   dfile(17)='omibufr',   dtype(17)='omi',       dplat(17)='aura',      dsis(17)='omi_aura',            dval(17)=1.0,  dthin(17)=6,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',           dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',           dval(19)=6.0,  dthin(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',           dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=6.0,  dthin(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',            dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',            dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',            dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',            dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',  dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=20.0, dthin(26)=1,
   dfile(27)='msubufr',   dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',             dval(27)=2.0,  dthin(27)=2,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',           dval(28)=10.0, dthin(28)=2,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',           dval(29)=0.0,  dthin(29)=2,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',           dval(30)=0.0,  dthin(30)=2,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',           dval(31)=10.0, dthin(31)=2,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=10.0, dthin(32)=2,
   dfile(33)='airsbufr',  dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',          dval(33)=5.0,  dthin(33)=2,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',           dval(34)=3.0,  dthin(34)=3,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',           dval(35)=3.0,  dthin(35)=3,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',           dval(36)=3.0,  dthin(36)=3,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',             dval(37)=3.0,  dthin(37)=3,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',         dval(38)=3.0,  dthin(38)=3,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',            dval(39)=0.0,  dthin(39)=4,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',            dval(40)=0.0,  dthin(40)=4,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',            dval(41)=0.0,  dthin(41)=4,
   dfile(42)='amsrebufr', dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',          dval(42)=0.0,  dthin(42)=4,
   dfile(43)='amsrebufr', dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=4,
   dfile(44)='amsrebufr', dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',          dval(44)=0.0,  dthin(44)=4,
   dfile(45)='ssmisbufr', dtype(45)='ssmis',     dplat(45)='f16',       dsis(45)='ssmis_f16',           dval(45)=0.0,  dthin(45)=4,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd1',    dplat(46)='g12',       dsis(46)='sndrD1_g12',          dval(46)=1.5,  dthin(46)=5,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd2',    dplat(47)='g12',       dsis(47)='sndrD2_g12',          dval(47)=1.5,  dthin(47)=5,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd3',    dplat(48)='g12',       dsis(48)='sndrD3_g12',          dval(48)=1.5,  dthin(48)=5,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd4',    dplat(49)='g12',       dsis(49)='sndrD4_g12',          dval(49)=1.5,  dthin(49)=5,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd1',    dplat(50)='g11',       dsis(50)='sndrD1_g11',          dval(50)=1.5,  dthin(50)=5,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd2',    dplat(51)='g11',       dsis(51)='sndrD2_g11',          dval(51)=1.5,  dthin(51)=5,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd3',    dplat(52)='g11',       dsis(52)='sndrD3_g11',          dval(52)=1.5,  dthin(52)=5,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd4',    dplat(53)='g11',       dsis(53)='sndrD4_g11',          dval(53)=1.5,  dthin(53)=5,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd1',    dplat(54)='g13',       dsis(54)='sndrD1_g13',          dval(54)=1.5,  dthin(54)=5,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd2',    dplat(55)='g13',       dsis(55)='sndrD2_g13',          dval(55)=1.5,  dthin(55)=5,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd3',    dplat(56)='g13',       dsis(56)='sndrD3_g13',          dval(56)=1.5,  dthin(56)=5,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd4',    dplat(57)='g13',       dsis(57)='sndrD4_g13',          dval(57)=1.5,  dthin(57)=5,
   dfile(58)='iasibufr',  dtype(58)='iasi',      dplat(58)='metop-a',   dsis(58)='iasi586_metop-a',     dval(58)=20.0, dthin(58)=1,
   dfile(59)='gomebufr',  dtype(59)='gome',      dplat(59)='metop-a',   dsis(59)='gome_metop-a',        dval(59)=1.0,  dthin(59)=6,
   dfile(60)='sbuvbufr',  dtype(60)='sbuv2',     dplat(60)='n19',       dsis(60)='sbuv8_n19',           dval(60)=1.0,  dthin(60)=0,  
   dfile(61)='hirs4bufr', dtype(61)='hirs4',     dplat(61)='n19',       dsis(61)='hirs4_n19',           dval(61)=6.0,  dthin(61)=1, 
   dfile(62)='amsuabufr', dtype(62)='amsua',     dplat(62)='n19',       dsis(62)='amsua_n19',           dval(62)=10.0, dthin(62)=2,
   dfile(63)='mhsbufr',   dtype(63)='mhs',       dplat(63)='n19',       dsis(63)='mhs_n19',             dval(63)=3.0,  dthin(63)=3,
   dfile(64)='tcvitl'     dtype(64)='tcp',       dplat(64)=' ',         dsis(64)='tcp',                 dval(64)=1.0,  dthin(64)=0,
   dfile(65)='mlsbufr',   dtype(65)='mls',       dplat(65)='aura',      dsis(65)='mls_aura',            dval(65)=1.0,  dthin(65)=0,
   dfile(66)='seviribufr',dtype(66)='seviri',    dplat(66)='m08',       dsis(66)='seviri_m08',          dval(66)=0.0,  dthin(66)=1,
   dfile(67)='seviribufr',dtype(67)='seviri',    dplat(67)='m09',       dsis(67)='seviri_m09',          dval(67)=0.0,  dthin(67)=1,
   dfile(68)='seviribufr',dtype(68)='seviri',    dplat(68)='m10',       dsis(68)='seviri_m10',          dval(68)=0.0,  dthin(68)=1,
   dfile(69)='hirs4bufr', dtype(69)='hirs4',     dplat(69)='metop-b',   dsis(69)='hirs4_metop-b',       dval(69)=0.0,  dthin(69)=1,
   dfile(70)='amsuabufr', dtype(70)='amsua',     dplat(70)='metop-b',   dsis(70)='amsua_metop-b',       dval(70)=0.0,  dthin(70)=1,
   dfile(71)='mhsbufr',   dtype(71)='mhs',       dplat(71)='metop-b',   dsis(71)='mhs_metop-b',         dval(71)=0.0,  dthin(71)=1,
   dfile(72)='iasibufr',  dtype(72)='iasi',      dplat(72)='metop-b',   dsis(72)='iasi616_metop-b',     dval(72)=0.0,  dthin(72)=1,
   dfile(73)='gomebufr',  dtype(73)='gome',      dplat(73)='metop-b',   dsis(73)='gome_metop-b',        dval(73)=0.0,  dthin(73)=1,
   dfile(74)='atmsbufr',  dtype(74)='atms',      dplat(74)='npp',       dsis(74)='atms_npp',            dval(74)=0.0,  dthin(74)=1,
   dfile(75)='crisbufr',  dtype(75)='cris',      dplat(75)='npp',       dsis(75)='cris_npp',            dval(75)=0.0,  dthin(75)=1,
   dfile(76)='modisbufr', dtype(76)='aodmodis',  dplat(76)='aqua',      dsis(76)='modis_aqua',          dval(76)=1.0,  dthin(65)=6,
   dfile(77)='modisbufr', dtype(77)='aodmodis',  dplat(77)='terra',     dsis(77)='modis_terra',         dval(77)=1.0,  dthin(77)=6,
 /
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${if_hybrid},uv_hyb_ens=.true.,
   aniso_a_en=.false.,generate_ens=.false.,
   n_ens=${ntotmem},
   beta1_inv=0.5,s_ens_h=110,s_ens_v=-0.3,
   regional_ensemble_option=${ensbk_option},
 /
 &RAPIDREFRESH_CLDSURF
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=37.,oblon=285.,obpres=500.,obdattim=${ANAL_TIME},
   obhourset=0.,
 /

EOF

#
###################################################
#  run  GSI
###################################################
echo ' Run GSI with' ${bk_core} 'background'

case $ARCH in
   'IBM_LSF'|'IBM_LoadLevel')
      ${RUN_COMMAND} ./gsi.exe < gsiparm.anl > stdout 2>&1  ;;

   * )
      ${RUN_COMMAND} ./gsi.exe > stdout 2>&1  ;;
esac

##################################################################
#  run time error check
##################################################################
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${GSI} crashed  Exit status=${error}"
  exit ${error}
fi
#
##################################################################
#
#   GSI updating satbias_in
#
# cp ./satbias_out ${FIX_ROOT}/ndas.t06z.satbias.tm03

# Copy the output to more understandable names
ln -s stdout      stdout.anl.${ANAL_TIME}
ln -s wrf_inout   wrfanl.${ANAL_TIME}
ln -s fort.201    fit_p1.${ANAL_TIME}
ln -s fort.202    fit_w1.${ANAL_TIME}
ln -s fort.203    fit_t1.${ANAL_TIME}
ln -s fort.204    fit_q1.${ANAL_TIME}
ln -s fort.207    fit_rad1.${ANAL_TIME}

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

ls -l pe0*.* > listpe
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="conv amsua_metop-a mhs_metop-a hirs4_metop-a hirs2_n14 msu_n14 \
          sndr_g08 sndr_g10 sndr_g12 sndr_g08_prep sndr_g10_prep sndr_g12_prep \
          sndrd1_g08 sndrd2_g08 sndrd3_g08 sndrd4_g08 sndrd1_g10 sndrd2_g10 \
          sndrd3_g10 sndrd4_g10 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 \
          hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 \
          amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua \
          goes_img_g08 goes_img_g10 goes_img_g11 goes_img_g12 \
          pcp_ssmi_dmsp pcp_tmi_trmm sbuv2_n16 sbuv2_n17 sbuv2_n18 \
          omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 amsua_n18 mhs_n18 \
          amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 \
          ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16"
   for type in $listall; do
      count=`grep pe*${type}_${loop}* listpe | wc -l`
      if [[ $count -gt 0 ]]; then
         cat pe*${type}_${loop}* > diag_${type}_${string}.${ANAL_TIME}
      fi
   done
done

#  Clean working directory to save only important files 
ls -l * > list_run_directory
if [ ${if_clean} = clean ]; then
  echo ' Clean working directory after GSI run'
  rm -f *Coeff.bin     # all CRTM coefficient files
  rm -f pe0*           # diag files on each processor
  rm -f listpe         # list of diag files on each processor
  rm -f obs_input.*    # observation middle files
  rm -f siganl sigf03  # background middle files
  rm -f xhatsave.*     # some information on each processor
  rm -f fsize_*        # delete temperal file for bufr size
fi

#exit 0

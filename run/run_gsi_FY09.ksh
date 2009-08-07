#!/bin/ksh

machine=LINDEN
machine=BLUEFIRE
if [ ${machine} = BLUEFIRE ] ; then
# NOTE:  To ensure reproducible results, must use same number of
#        MPI tasks AND nodes for each run.  blocking=unlimited
#        leads to roundoff differences in mpi_allreduce.

## Below are LoadLeveler (IBM queueing system) commands
#BSUB -P 48503002
#BSUB -a poe                         # at NCAR: bluevista
#BSUB -x                                # exlusive use of node (not_shared)
#BSUB -n   12                              # number of total tasks
#BSUB -R "span[ptile=2]"                # how many tasks per node (up to 8)
#BSUB -J gsi                           # job name
#BSUB -o gsi.out                       # output filename (%J to add job id)
#BSUB -e gsi.err                       # error filename
#BSUB -W 00:02
#BSUB -q regular                         # queue

set -x

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes


# Set environment variables for threads
export SPINLOOPTIME=10000
export YIELDLOOPTIME=40000
export AIXTHREAD_SCOPE=S
export MALLOCMULTIHEAP=true
export XLSMPOPTS="parthds=1:spins=0:yields=0:stack=128000000"


# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

fi
#
##################################################################################
# set up path and file that need for GSI analysis
#
# analysis time  (YYYYMMDDHH)
  ANAL_TIME=2008051112
# working direcotry, where GSI runs
  WORK_ROOT=/d1/mhu/gsi/case

# path and name of background file
  BK_FILE=/d1/mhu/gsi/case/2008051112/bkARW/wrfout_d01_2008-05-11_12:00:00
# path of observations 
  OBS_ROOT=/d1/mhu/gsi/case
  PREPBUFR=/d1/mhu/gsi/tutorialcases/data/newgblav.gdas1.t12z.prepbufr.nr
# path of fix files
  FIX_ROOT=/home/mhu/GSI/GSI_trunk_10_Q1FY09/fix

# path and name of the gsi executable 
  GSI_EXE=/home/mhu/GSI/GSI_trunk_10_Q1FY09/sorc/global_gsi

# which background error covariance and parameter will be used (GLOBAL or NAM)
  bkcv_option=NAM

if [ ${machine} = BLUEFIRE ] ; then
  ANAL_TIME=2007081500
  WORK_ROOT=/ptmp/mhu/test_Q1FY09/run/tmpreg_arw_${ANAL_TIME}
  PREPBUFR=/ptmp/mhu/blueice/t8/obs/gdas1.t00z.prepbufr.nr
  BK_FILE=/ptmp/mhu/blueice/t8/t8_case_2007081500/wrfinput_d01_2007081500
  OBS_ROOT=/ptmp/mhu/blueice/t8/obs
  FIX_ROOT=/ptmp/mhu/test_Q1FY09/fix
  GSI_EXE=/ptmp/mhu/test_Q1FY09/sorc/global_gsi.fd/global_gsi
fi

#
##################################################################################
# Set up running environment 
  echo ${machine}
#
# processor number used for GSI analysis
  GSIPROC=1

# mpi
  MPIRUN=mpirun

# Set endian conversion options for use with Intel compilers
##  export F_UFMTENDIAN="big;little:10,15,66"
##  export F_UFMTENDIAN="big;little:10,13,15,66"
##  export GMPIENVVAR=F_UFMTENDIAN

 export MV2_ON_DEMAND_THRESHOLD=256

#
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
if [ ! -d "${WORK_ROOT}" ]; then
  echo "create WORK_ROOT directory "
  mkdir ${WORK_ROOT}
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
  workdir=${WORK_ROOT}/gsiprd
  rm -rf ${workdir}
  mkdir -p ${workdir}
  cd ${workdir}
  echo ${workdir}

#
##################################################################################
# Save a copy of the GSI executable in the workdir
  cp ${GSI_EXE} gsi.exe

# Bring over background field (it's modified by GSI so we can't link to it)
cp ${BK_FILE} ./wrf_inout


# Link to the prepbufr data
ln -s ${PREPBUFR} ./prepbufr

# Link to the radiance data
touch amsuabufr
touch amsubbufr
touch hirs3bufr
touch hirs4bufr
touch mhsbufr
touch   airsbufr
touch   tmibufr
touch   sbuvbufr
touch   amsrebufr
touch   ssmibufr
touch   ssmisbufr
touch   ssmitbufr
touch   l2rwbufr
touch   gpsbufr
touch   gimgrbufr
touch   hirs2bufr
touch   gsndrbufr
touch   gsnd1bufr


#
##################################################################################
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
  BERROR=${FIX_ROOT}/nam_regional_glb_berror.f77
  SATANGL=${FIX_ROOT}/global_satangbias.txt
  SATINFO=${FIX_ROOT}/global_satinfo.txt
  OBERROR=${FIX_ROOT}/prepobs_errtable.global
  CONVINFO=${FIX_ROOT}/global_convinfo.txt
  OZINFO=${FIX_ROOT}/global_ozinfo.txt
  PCPINFO=${FIX_ROOT}/global_pcpinfo.txt
else
  echo ' Use NAM background error covariance'
  SATANGL=${FIX_ROOT}/nam_global_satangbias.txt
  SATINFO=${FIX_ROOT}/nam_regional_satinfo.txt
  BERROR=${FIX_ROOT}/nam_nmmstat_na
  OBERROR=${FIX_ROOT}/nam_errtable.r3dv
  CONVINFO=${FIX_ROOT}/nam_regional_convinfo.txt
  OZINFO=${FIX_ROOT}/nam_global_ozinfo.txt
  PCPINFO=${FIX_ROOT}/nam_global_pcpinfo.txt
fi

RTMFIX=${FIX_ROOT}/crtm_gfsgsi
# For Big_Endian (Linux, pgf) or Little_Endian (IBM, and linux ifort)
if [ ${machine} = BLUEFIRE ]; then
  echo " link to little endian fix files"
##  BYTE_ORDER=Little_Endian
  BYTE_ORDER=Big_Endian
else
  echo " link to Big endian fix files"
  BYTE_ORDER=Big_Endian 
fi
RTMEMIS=${RTMFIX}/EmisCoeff/${BYTE_ORDER}/EmisCoeff.bin
RTMAERO=${RTMFIX}/AerosolCoeff/${BYTE_ORDER}/AerosolCoeff.bin
RTMCLDS=${RTMFIX}/CloudCoeff/${BYTE_ORDER}/CloudCoeff.bin

#  copy Fixed fields to working directory
 cp $BERROR   berror_stats
 cp $SATANGL  satbias_angle
 cp $SATINFO  satinfo
 cp $RTMEMIS  EmisCoeff.bin
 cp $RTMAERO  AerosolCoeff.bin
 cp $RTMCLDS  CloudCoeff.bin
 cp $CONVINFO convinfo
 cp $OZINFO   ozinfo
 cp $PCPINFO  pcpinfo
 cp $OBERROR  errtable
# 
#    # CRTM Spectral and Transmittance coefficients
    nsatsen=`cat satinfo | wc -l`
    isatsen=1
    while [[ $isatsen -le $nsatsen ]]; do
       flag=`head -n $isatsen satinfo | tail -1 | cut -c1-1`
       if [[ "$flag" != "!" ]]; then
          satsen=`head -n $isatsen satinfo | tail -1 | cut -f 2 -d" "`
          spccoeff=${satsen}.SpcCoeff.bin
          if  [[ ! -s $spccoeff ]]; then
             cp $RTMFIX/SpcCoeff/${BYTE_ORDER}/$spccoeff $spccoeff
             cp $RTMFIX/TauCoeff/${BYTE_ORDER}/${satsen}.TauCoeff.bin ${satsen}.TauCoeff.bin
          fi
       fi
       isatsen=` expr $isatsen + 1 `
    done

# Only need this file for single obs test
 bufrtable=${FIX_ROOT}/prepobs_prep.bufrtable
 cp $bufrtable ./prepobs_prep.bufrtable

# fr satellite bias correction
cp ${FIX_ROOT}/ndas.t06z.satbias.tm03 ./satbias_in

#
##################################################################################
# Set some parameters for use by the GSI executable and to build the namelist
export JCAP=62
export LEVS=60
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}

if [ ${bkcv_option} = GLOBAL ] ; then
   as_op='0.6,0.6,0.75,0.75,0.75,0.75,1.0,1.0'
   vs_op='0.7,'
   hzscl_op='1.7,0.8,0.5,'
else
   as_op='1.0,1.0,0.5 ,0.7,0.7,0.5,1.0,1.0,'
   vs_op='1.0,'
   hzscl_op='0.373,0.746,1.50,'
fi
# Build the GSI namelist on-the-fly
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=5,niter(2)=5,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=1,
   gencode=78,factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=59,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,
   $SETUP
 /     
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.true.,diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.true.,
   regional=.true.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   as=${as_op}
   vs=${vs_op}
   hzscl=${hzscl_op}
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.false.,bkgv_rewgtfct=1.5
   $BKGVERR
/
 &ANBKGERR
   anisotropic=.false.,
 /
 &JCOPTS
   jcterm=.false.,jcdivt=.false.,bamp_ext1=2.5e12,bamp_ext2=5.0e11,
   bamp_int1=2.5e13,bamp_int2=2.5e12,
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',         dsis(04)='pw',                  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='uv',        dplat(05)=' ',         dsis(05)='uv',                  dval(05)=1.0,  dthin(05)=0,
   dfile(06)='prepbufr',  dtype(06)='spd',       dplat(06)=' ',         dsis(06)='spd',                 dval(06)=1.0,  dthin(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',                  dval(07)=1.0,  dthin(07)=0,
   dfile(08)='radarbufr', dtype(08)='rw',        dplat(08)=' ',         dsis(08)='rw',                  dval(08)=1.0,  dthin(08)=0,
   dfile(09)='prepbufr',  dtype(09)='sst',       dplat(09)=' ',         dsis(09)='sst',                 dval(09)=1.0,  dthin(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',             dval(10)=1.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',            dval(11)=1.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',             dval(12)=1.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',           dval(13)=1.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',           dval(14)=1.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',           dval(15)=1.0,  dthin(15)=0,
   dfile(16)='hirs2bufr', dtype(16)='hirs2',     dplat(16)='n14',       dsis(16)='hirs2_n14',           dval(16)=6.0,  dthin(16)=1,
   dfile(17)='hirs3bufr', dtype(17)='hirs3',     dplat(17)='n16',       dsis(17)='hirs3_n16',           dval(17)=0.0,  dthin(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n17',       dsis(18)='hirs3_n17',           dval(18)=6.0,  dthin(18)=1,
   dfile(19)='hirs4bufr', dtype(19)='hirs4',     dplat(19)='n18',       dsis(19)='hirs4_n18',           dval(19)=0.0,  dthin(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='metop-a',   dsis(20)='hirs4_metop-a',       dval(20)=6.0,  dthin(20)=1,
   dfile(21)='gsndrbufr', dtype(21)='sndr',      dplat(21)='g11',       dsis(21)='sndr_g11',            dval(21)=0.0,  dthin(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g12',       dsis(22)='sndr_g12',            dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gimgrbufr', dtype(23)='goes_img',  dplat(23)='g11',       dsis(23)='imgr_g11',            dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g12',       dsis(24)='imgr_g12',            dval(24)=0.0,  dthin(24)=1,
   dfile(25)='airsbufr',  dtype(25)='airs',      dplat(25)='aqua',      dsis(25)='airs281SUBSET_aqua',  dval(25)=20.0, dthin(25)=1,
   dfile(26)='msubufr',   dtype(26)='msu',       dplat(26)='n14',       dsis(26)='msu_n14',             dval(26)=2.0,  dthin(26)=2,
   dfile(27)='amsuabufr', dtype(27)='amsua',     dplat(27)='n15',       dsis(27)='amsua_n15',           dval(27)=10.0, dthin(27)=2,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n16',       dsis(28)='amsua_n16',           dval(28)=0.0,  dthin(28)=2,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n17',       dsis(29)='amsua_n17',           dval(29)=0.0,  dthin(29)=2,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n18',       dsis(30)='amsua_n18',           dval(30)=10.0, dthin(30)=2,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='metop-a',   dsis(31)='amsua_metop-a',       dval(31)=10.0, dthin(31)=2,
   dfile(32)='airsbufr',  dtype(32)='amsua',     dplat(32)='aqua',      dsis(32)='amsua_aqua',          dval(32)=5.0,  dthin(32)=2,
   dfile(33)='amsubbufr', dtype(33)='amsub',     dplat(33)='n15',       dsis(33)='amsub_n15',           dval(33)=3.0,  dthin(33)=3,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n16',       dsis(34)='amsub_n16',           dval(34)=3.0,  dthin(34)=3,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n17',       dsis(35)='amsub_n17',           dval(35)=3.0,  dthin(35)=3,
   dfile(36)='mhsbufr',   dtype(36)='mhs',       dplat(36)='n18',       dsis(36)='mhs_n18',             dval(36)=3.0,  dthin(36)=3,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='metop-a',   dsis(37)='mhs_metop-a',         dval(37)=3.0,  dthin(37)=3,
   dfile(38)='ssmitbufr', dtype(38)='ssmi',      dplat(38)='f13',       dsis(38)='ssmi_f13',            dval(38)=0.0,  dthin(38)=4,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f14',       dsis(39)='ssmi_f14',            dval(39)=0.0,  dthin(39)=4,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f15',       dsis(40)='ssmi_f15',            dval(40)=0.0,  dthin(40)=4,
   dfile(41)='amsrebufr', dtype(41)='amsre_low', dplat(41)='aqua',      dsis(41)='amsre_aqua',          dval(41)=0.0,  dthin(41)=4,
   dfile(42)='amsrebufr', dtype(42)='amsre_mid', dplat(42)='aqua',      dsis(42)='amsre_aqua',          dval(42)=0.0,  dthin(42)=4,
   dfile(43)='amsrebufr', dtype(43)='amsre_hig', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=4,
   dfile(44)='ssmisbufr', dtype(44)='ssmis_las', dplat(44)='f16',       dsis(44)='ssmis_f16',           dval(44)=0.0,  dthin(44)=4,
   dfile(45)='ssmisbufr', dtype(45)='ssmis_uas', dplat(45)='f16',       dsis(45)='ssmis_f16',           dval(45)=0.0,  dthin(45)=4,
   dfile(46)='ssmisbufr', dtype(46)='ssmis_img', dplat(46)='f16',       dsis(46)='ssmis_f16',           dval(46)=0.0,  dthin(46)=4,
   dfile(47)='ssmisbufr', dtype(47)='ssmis_env', dplat(47)='f16',       dsis(47)='ssmis_f16',           dval(47)=0.0,  dthin(47)=4,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd1',    dplat(48)='g12',       dsis(48)='sndrD1_g12',          dval(48)=1.5,  dthin(48)=5,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd2',    dplat(49)='g12',       dsis(49)='sndrD2_g12',          dval(49)=1.5,  dthin(49)=5,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd3',    dplat(50)='g12',       dsis(50)='sndrD3_g12',          dval(50)=1.5,  dthin(50)=5,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd4',    dplat(51)='g12',       dsis(51)='sndrD4_g12',          dval(51)=1.5,  dthin(51)=5,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd1',    dplat(52)='g11',       dsis(52)='sndrD1_g11',          dval(52)=1.5,  dthin(52)=5,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd2',    dplat(53)='g11',       dsis(53)='sndrD2_g11',          dval(53)=1.5,  dthin(53)=5,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd3',    dplat(54)='g11',       dsis(54)='sndrD3_g11',          dval(54)=1.5,  dthin(54)=5,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd4',    dplat(55)='g11',       dsis(55)='sndrD4_g11',          dval(55)=1.5,  dthin(55)=5,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd1',    dplat(56)='g13',       dsis(56)='sndrD1_g13',          dval(56)=1.5,  dthin(56)=5,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd2',    dplat(57)='g13',       dsis(57)='sndrD2_g13',          dval(57)=1.5,  dthin(57)=5,
   dfile(58)='gsnd1bufr', dtype(58)='sndrd3',    dplat(58)='g13',       dsis(58)='sndrD3_g13',          dval(58)=1.5,  dthin(58)=5,
   dfile(59)='gsnd1bufr', dtype(59)='sndrd4',    dplat(59)='g13',       dsis(59)='sndrD4_g13',          dval(59)=1.5,  dthin(59)=5,

 /
 &SUPEROB_RADAR
   $SUPERRAD
 /
 &SINGLEOB_TEST
   maginnov=10.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /
EOF

#
##################################################################################
cp ${FIX_ROOT}/ndas.t06z.satbias.tm03 ./satbias_in

if [ ${machine} = BLUEFIRE ] ; then
  mpirun.lsf  ${workdir}/gsi.exe < gsiparm.anl > stdout
else
  ${MPIRUN} -np ${GSIPROC} gsi.exe < gsiparm.anl > stdout 2>&1
fi
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${GSI} crashed  Exit status=${error}"
  exit ${error}
fi

# Look for successful completion messages in rsl files
nsuccess=`tail -l stdout | awk '/PROGRAM GSI_ANL HAS ENDED/' | wc -l`
ntotal=1 
echo "Found ${nsuccess} of ${ntotal} completion messages"
if [ ${nsuccess} -ne ${ntotal} ]; then
   echo "ERROR: ${GSI} did not complete sucessfully  Exit status=${error}"
   if [ ${error} -ne 0 ]; then
     exit ${error}
   else
     exit 1
   fi
fi

#
##################################################################################
#
#   GSI updating satbias_in
#
cp ./satbias_out ${FIX_ROOT}/ndas.t06z.satbias.tm03

# Copy the output to more understandable names
cp stdout      stdout.anl.${ANAL_TIME}
cp wrf_inout   wrfanl.${ANAL_TIME}
ln fort.201    fit_p1.${ANAL_TIME}
ln fort.202    fit_w1.${ANAL_TIME}
ln fort.203    fit_t1.${ANAL_TIME}
ln fort.204    fit_q1.${ANAL_TIME}
ln fort.207    fit_rad1.${ANAL_TIME}

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

cd $DATA    # we should already be in $DATA, but extra cd to be sure.
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="conv hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls pe*${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat pe*${type}_${loop}* > diag_${type}_${string}.${adate}
      fi
   done
done


exit 0

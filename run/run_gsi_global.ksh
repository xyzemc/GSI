#!/bin/ksh --login
#####################################################
# machine set up (users should change this part)
#####################################################
#
# Set the queueing options 
#PBS -l procs=24
#PBS -l walltime=0:20:00
#PBS -A zrtrr
#PBS -N global_gsi
#PBS -j oe

set -x
np=$PBS_NP

module load intel
module load mpt

#
# GSIPROC = processor number used for GSI analysis
#------------------------------------------------
  GSIPROC=${np}
  ARCH='LINUX_Intel_PBS'
# Supported configurations:
            # IBM_LSF,
            # LINUX_Intel, LINUX_Intel_LSF, LINUX_Intel_PBS,
            # LINUX_PGI, LINUX_PGI_LSF, LINUX_PGI_PBS,
            # DARWIN_PGI
#
#####################################################
# case set up (users should change this part)
#####################################################
#
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working directory, where GSI runs
# PREPBURF = path of PreBUFR conventional obs
# BK_FILE  = path and name of background file
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# GSI_EXE  = path and name of the gsi executable 
  ANAL_TIME=2011080100
  GUESS_TIME=2011073118
  WORK_ROOT=/scratch1/portfolios/BMC/wrfruc/mhu/code/comGSI/trunk_r1059/run/gsiprd_${ANAL_TIME}_gfs
  BK_ROOT=/scratch2/portfolios/BMC/zrtrr/mhu/data/global/2011080100
  OBS_ROOT=/scratch2/portfolios/BMC/zrtrr/mhu/data/global/2011080100
  PREPBUFR=${OBS_ROOT}/gdas1.t00z.prepbufr.nr
  FIX_ROOT=/scratch1/portfolios/BMC/wrfruc/mhu/code/comGSI/trunk_r1059/fix
  CRTM_ROOT=/scratch1/portfolios/BMC/comgsi/gsilibs/CRTM_REL-2.0.5/CRTM_Coefficients-2.0.5
  GSI_EXE=/scratch1/portfolios/BMC/wrfruc/mhu/code/comGSI/trunk_r1059/run/gsi.exe

#------------------------------------------------
# bk_core= which WRF core is used as background (NMM or ARW)
# bkcv_option= which background error covariance and parameter will be used 
#              (GLOBAL or NAM)
# if_clean = clean  : delete temperal files in working directory (default)
#            no     : leave running directory as is (this is for debug only)
  bk_core=ARW
  bkcv_option=NAM
  if_clean=no

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
  JCAP=62
  LEVS=64
  JCAP_B=62
#
#
#####################################################
# Users should NOT change script after this point
#####################################################
#
BYTE_ORDER_CRTM=Big_Endian

case $ARCH in
   'IBM_LSF')
      ###### IBM LSF (Load Sharing Facility)
      BYTE_ORDER=Big_Endian
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_Intel')
      BYTE_ORDER=Little_Endian
      if [ $GSIPROC = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
        RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   'LINUX_Intel_LSF')
      ###### LINUX LSF (Load Sharing Facility)
      BYTE_ORDER=Little_Endian
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_Intel_PBS')
      BYTE_ORDER=Big_Endian
      #### Linux cluster PBS (Portable Batch System)
      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;

   'LINUX_PGI')
      BYTE_ORDER=Little_Endian
      if [ $GSIPROC = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
         RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   'LINUX_PGI_LSF')
      ###### LINUX LSF (Load Sharing Facility)
      BYTE_ORDER=Little_Endian
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_PGI_PBS')
      BYTE_ORDER=Little_Endian
      ###### Linux cluster PBS (Portable Batch System)
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
if [ ! -r "${BK_ROOT}" ]; then
  echo "ERROR: ${BK_ROOT} does not exist!"
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

################################################################################
## Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
hha=`echo $ANAL_TIME | cut -c9-10`
hhg=`echo $GUESS_TIME | cut -c9-10`

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
# Set some parameters for use by the GSI executable and to build the namelist
echo " Build the namelist "

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   LONA=768
   LATA=384
   DELTIM=180
   resol=1
elif [[ "$JCAP" = "62" ]]; then
   LONA=192
   LATA=94
   DELTIM=1200
   resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
NLAT=` expr $LATA + 2 `

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${FIX_ROOT}
        yyyy=`echo $ANAL_TIME | cut -c1-4`
        rm ./global_co2_data.txt
        co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        while [ ! -s $co2 ] ; do
                ((yyyy-=1))
                co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        done
        if [ -s $co2 ] ; then
                cp $co2 ./global_co2_data.txt
        fi
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
        fi
fi
#CH4 file decision
ICH4=${ICH4:-0}
if [ $ICH4 -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        ch4dir=${FIX_ROOT}
        yyyy=`echo $ANAL_TIME | cut -c1-4`
        rm ./ch4globaldata.txt
        ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        while [ ! -s $ch4 ] ; do
                ((yyyy-=1))
                ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        done
        if [ -s $ch4 ] ; then
                cp $ch4 ./ch4globaldata.txt
        fi
        if [ ! -s ./ch4globaldata.txt ] ; then
                echo "\./ch4globaldata.txt" not created
                exit 1
        fi
fi
IN2O=${IN2O:-0}
if [ $IN2O -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        n2odir=${FIX_ROOT}
        yyyy=`echo $ANAL_TIME | cut -c1-4`
        rm ./n2oglobaldata.txt
        n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        while [ ! -s $n2o ] ; do
                ((yyyy-=1))
                n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        done
        if [ -s $n2o ] ; then
                cp $n2o ./n2oglobaldata.txt
        fi
        if [ ! -s ./n2oglobaldata.txt ] ; then
                echo "\./n2oglobaldata.txt" not created
                exit 1
        fi
fi
ICO=${ICO:-0}
if [ $ICO -gt 0 ] ; then
#        # Copy CO files to $tmpdir
        codir=${FIX_ROOT}
        yyyy=`echo $ANAL_TIME | cut -c1-4`
        rm ./coglobaldata.txt
        co=$codir/global_co_esrlctm_$yyyy.txt
        while [ ! -s $co ] ; do
                ((yyyy-=1))
                co=$codir/global_co_esrlctm_$yyyy.txt
        done
        if [ -s $co ] ; then
                cp $co ./coglobaldata.txt
        fi
        if [ ! -s ./coglobaldata.txt ] ; then
                echo "\./coglobaldata.txt" not created
                exit 1
        fi
fi

GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""

# Build the GSI namelist on-the-fly
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=10,niter(2)=10,
   niter_no_qc(1)=1,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=82,qoption=2,
   factqmin=5.0,factqmax=5.0,deltim=$DELTIM,
   ndat=67,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=.false.,lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,
   $JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=1,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   tlnmc_type=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=1450.0,dmesh(2)=1500.0,time_window_max=0.5,
   dfile(01)='prepbufr',       dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',                dval(01)=0.0,  dthin(01)=0,  dsfcalc(01)=0,
   dfile(02)='prepbufr'        dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',                 dval(02)=0.0,  dthin(02)=0,  dsfcalc(02)=0,
   dfile(03)='prepbufr',       dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',                 dval(03)=0.0,  dthin(03)=0,  dsfcalc(03)=0,
   dfile(04)='prepbufr',       dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',                dval(04)=0.0,  dthin(04)=0,  dsfcalc(04)=0,
   dfile(05)='prepbufr',       dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',                dval(05)=0.0,  dthin(05)=0,  dsfcalc(05)=0,
   dfile(06)='satwnd',         dtype(06)='uv',        dplat(06)=' ',       dsis(06)='uv',                dval(06)=0.0,  dthin(06)=0,  dsfcalc(06)=0,
   dfile(07)='prepbufr',       dtype(07)='spd',       dplat(07)=' ',       dsis(07)='spd',               dval(07)=0.0,  dthin(07)=0,  dsfcalc(07)=0,
   dfile(08)='prepbufr',       dtype(08)='dw',        dplat(08)=' ',       dsis(08)='dw',                dval(08)=0.0,  dthin(08)=0,  dsfcalc(08)=0,
   dfile(09)='radarbufr',      dtype(09)='rw',        dplat(09)=' ',       dsis(09)='rw',                dval(09)=0.0,  dthin(09)=0,  dsfcalc(09)=0,
   dfile(10)='prepbufr',       dtype(10)='sst',       dplat(10)=' ',       dsis(10)='sst',               dval(10)=0.0,  dthin(10)=0,  dsfcalc(10)=0,
   dfile(11)='gpsrobufr',      dtype(11)='$gps_dtype',   dplat(11)=' ',       dsis(11)='gps',               dval(11)=0.0,  dthin(11)=0,  dsfcalc(11)=0,
   dfile(12)='ssmirrbufr',     dtype(12)='pcp_ssmi',  dplat(12)='dmsp',    dsis(12)='pcp_ssmi',          dval(12)=0.0,  dthin(12)=-1, dsfcalc(12)=0,
   dfile(13)='tmirrbufr',      dtype(13)='pcp_tmi',   dplat(13)='trmm',    dsis(13)='pcp_tmi',           dval(13)=0.0,  dthin(13)=-1, dsfcalc(13)=0,
   dfile(14)='sbuvbufr',       dtype(14)='sbuv2',     dplat(14)='n16',     dsis(14)='sbuv8_n16',         dval(14)=0.0,  dthin(14)=0,  dsfcalc(14)=0,
   dfile(15)='sbuvbufr',       dtype(15)='sbuv2',     dplat(15)='n17',     dsis(15)='sbuv8_n17',         dval(15)=0.0,  dthin(15)=0,  dsfcalc(15)=0,
   dfile(16)='sbuvbufr',       dtype(16)='sbuv2',     dplat(16)='n18',     dsis(16)='sbuv8_n18',         dval(16)=0.0,  dthin(16)=0,  dsfcalc(16)=0,
   dfile(17)='hirs3bufr',      dtype(17)='hirs3',     dplat(17)='n17',     dsis(17)='hirs3_n17',         dval(17)=0.0,  dthin(17)=1,  dsfcalc(17)=1,
   dfile(18)='hirs4bufr_skip', dtype(18)='hirs4',     dplat(18)='metop-a', dsis(18)='hirs4_metop-a',     dval(18)=0.0,  dthin(18)=1,  dsfcalc(18)=1,
   dfile(19)='gimgrbufr',      dtype(19)='goes_img',  dplat(19)='g11',     dsis(19)='imgr_g11',          dval(19)=0.0,  dthin(19)=1,  dsfcalc(19)=0,
   dfile(20)='gimgrbufr',      dtype(20)='goes_img',  dplat(20)='g12',     dsis(20)='imgr_g12',          dval(20)=0.0,  dthin(20)=1,  dsfcalc(20)=0,
   dfile(21)='airsbufr',       dtype(21)='airs',      dplat(21)='aqua',    dsis(21)='airs281SUBSET_aqua',dval(21)=0.0,  dthin(21)=1,  dsfcalc(21)=1,
   dfile(22)='amsuabufr_skip', dtype(22)='amsua',     dplat(22)='n15',     dsis(22)='amsua_n15',         dval(22)=0.0,  dthin(22)=1,  dsfcalc(22)=1,
   dfile(23)='amsuabufr_skip', dtype(23)='amsua',     dplat(23)='n18',     dsis(23)='amsua_n18',         dval(23)=0.0,  dthin(23)=1,  dsfcalc(23)=1,
   dfile(24)='amsuabufr_skip', dtype(24)='amsua',     dplat(24)='metop-a', dsis(24)='amsua_metop-a',     dval(24)=0.0,  dthin(24)=1,  dsfcalc(24)=1,
   dfile(25)='airsbufr_skip',  dtype(25)='amsua',     dplat(25)='aqua',    dsis(25)='amsua_aqua',        dval(25)=0.0,  dthin(25)=1,  dsfcalc(25)=1,
   dfile(26)='amsubbufr',      dtype(26)='amsub',     dplat(26)='n17',     dsis(26)='amsub_n17',         dval(26)=0.0,  dthin(26)=1,  dsfcalc(26)=1,
   dfile(27)='mhsbufr_skip',   dtype(27)='mhs',       dplat(27)='n18',     dsis(27)='mhs_n18',           dval(27)=0.0,  dthin(27)=1,  dsfcalc(27)=1,
   dfile(28)='mhsbufr_skip',   dtype(28)='mhs',       dplat(28)='metop-a', dsis(28)='mhs_metop-a',       dval(28)=0.0,  dthin(28)=1,  dsfcalc(28)=1,
   dfile(29)='ssmitbufr',      dtype(29)='ssmi',      dplat(29)='f14',     dsis(29)='ssmi_f14',          dval(29)=0.0,  dthin(29)=1,  dsfcalc(29)=0,
   dfile(30)='ssmitbufr',      dtype(30)='ssmi',      dplat(30)='f15',     dsis(30)='ssmi_f15',          dval(30)=0.0,  dthin(30)=1,  dsfcalc(30)=0,
   dfile(31)='amsrebufr',      dtype(31)='amsre_low', dplat(31)='aqua',    dsis(31)='amsre_aqua',        dval(31)=0.0,  dthin(31)=1,  dsfcalc(31)=0,
   dfile(32)='amsrebufr',      dtype(32)='amsre_mid', dplat(32)='aqua',    dsis(32)='amsre_aqua',        dval(32)=0.0,  dthin(32)=1,  dsfcalc(32)=0,
   dfile(33)='amsrebufr',      dtype(33)='amsre_hig', dplat(33)='aqua',    dsis(33)='amsre_aqua',        dval(33)=0.0,  dthin(33)=1,  dsfcalc(33)=0,
   dfile(34)='ssmisbufr',      dtype(34)='ssmis_las', dplat(34)='f16',     dsis(34)='ssmis_f16',         dval(34)=0.0,  dthin(34)=1,  dsfcalc(34)=0,
   dfile(35)='ssmisbufr',      dtype(35)='ssmis_uas', dplat(35)='f16',     dsis(35)='ssmis_f16',         dval(35)=0.0,  dthin(35)=1,  dsfcalc(35)=0,
   dfile(36)='ssmisbufr',      dtype(36)='ssmis_img', dplat(36)='f16',     dsis(36)='ssmis_f16',         dval(36)=0.0,  dthin(36)=1,  dsfcalc(36)=0,
   dfile(37)='ssmisbufr',      dtype(37)='ssmis_env', dplat(37)='f16',     dsis(37)='ssmis_f16',         dval(37)=0.0,  dthin(37)=1,  dsfcalc(37)=0,
   dfile(38)='gsnd1bufr_skip', dtype(38)='sndrd1',    dplat(38)='g12',     dsis(38)='sndrD1_g12',        dval(38)=0.0,  dthin(38)=1,  dsfcalc(38)=0,
   dfile(39)='gsnd1bufr_skip', dtype(39)='sndrd2',    dplat(39)='g12',     dsis(39)='sndrD2_g12',        dval(39)=0.0,  dthin(39)=1,  dsfcalc(39)=0,
   dfile(40)='gsnd1bufr_skip', dtype(40)='sndrd3',    dplat(40)='g12',     dsis(40)='sndrD3_g12',        dval(40)=0.0,  dthin(40)=1,  dsfcalc(40)=0,
   dfile(41)='gsnd1bufr_skip', dtype(41)='sndrd4',    dplat(41)='g12',     dsis(41)='sndrD4_g12',        dval(41)=0.0,  dthin(41)=1,  dsfcalc(41)=0,
   dfile(42)='gsnd1bufr_skip', dtype(42)='sndrd1',    dplat(42)='g11',     dsis(42)='sndrD1_g11',        dval(42)=0.0,  dthin(42)=1,  dsfcalc(42)=0,
   dfile(43)='gsnd1bufr_skip', dtype(43)='sndrd2',    dplat(43)='g11',     dsis(43)='sndrD2_g11',        dval(43)=0.0,  dthin(43)=1,  dsfcalc(43)=0,
   dfile(44)='gsnd1bufr',      dtype(44)='sndrd3',    dplat(44)='g11',     dsis(44)='sndrD3_g11',        dval(44)=0.0,  dthin(44)=1,  dsfcalc(44)=0,
   dfile(45)='gsnd1bufr_skip', dtype(45)='sndrd4',    dplat(45)='g11',     dsis(45)='sndrD4_g11',        dval(45)=0.0,  dthin(45)=1,  dsfcalc(45)=0,
   dfile(46)='gsnd1bufr_skip', dtype(46)='sndrd1',    dplat(46)='g13',     dsis(46)='sndrD1_g13',        dval(46)=0.0,  dthin(46)=1,  dsfcalc(46)=0,
   dfile(47)='gsnd1bufr_skip', dtype(47)='sndrd2',    dplat(47)='g13',     dsis(47)='sndrD2_g13',        dval(47)=0.0,  dthin(47)=1,  dsfcalc(47)=0,
   dfile(48)='gsnd1bufr_skip', dtype(48)='sndrd3',    dplat(48)='g13',     dsis(48)='sndrD3_g13',        dval(48)=0.0,  dthin(48)=1,  dsfcalc(48)=0,
   dfile(49)='gsnd1bufr_skip', dtype(49)='sndrd4',    dplat(49)='g13',     dsis(49)='sndrD4_g13',        dval(49)=0.0,  dthin(49)=1,  dsfcalc(49)=0,
   dfile(50)='iasibufr',       dtype(50)='iasi',      dplat(50)='metop-a', dsis(50)='iasi616_metop-a',   dval(50)=0.0,  dthin(50)=1,  dsfcalc(50)=1,
   dfile(51)='gomebufr',       dtype(51)='gome',      dplat(51)='metop-a', dsis(51)='gome_metop-a',      dval(51)=0.0,  dthin(51)=2,  dsfcalc(51)=0,
   dfile(52)='omibufr',        dtype(52)='omi',       dplat(52)='aura',    dsis(52)='omi_aura',          dval(52)=0.0,  dthin(52)=2,  dsfcalc(52)=0,
   dfile(53)='sbuvbufr',       dtype(53)='sbuv2',     dplat(53)='n19',     dsis(53)='sbuv8_n19',         dval(53)=0.0,  dthin(53)=0,  dsfcalc(53)=0,
   dfile(54)='hirs4bufr',      dtype(54)='hirs4',     dplat(54)='n19',     dsis(54)='hirs4_n19',         dval(54)=0.0,  dthin(54)=1,  dsfcalc(54)=1,
   dfile(55)='amsuabufr',      dtype(55)='amsua',     dplat(55)='n19',     dsis(55)='amsua_n19',         dval(55)=0.0,  dthin(55)=1,  dsfcalc(55)=1,
   dfile(56)='mhsbufr',        dtype(56)='mhs',       dplat(56)='n19',     dsis(56)='mhs_n19',           dval(56)=0.0,  dthin(56)=1,  dsfcalc(56)=1,
   dfile(57)='tcvitl'          dtype(57)='tcp',       dplat(57)=' ',       dsis(57)='tcp',               dval(57)=0.0,  dthin(57)=0,  dsfcalc(57)=0,
   dfile(58)='seviribufr',     dtype(58)='seviri',    dplat(58)='m08',     dsis(58)='seviri_m08',        dval(58)=0.0,  dthin(58)=1,  dsfcalc(58)=0,
   dfile(59)='seviribufr',     dtype(59)='seviri',    dplat(59)='m09',     dsis(59)='seviri_m09',        dval(59)=0.0,  dthin(59)=1,  dsfcalc(59)=0,
   dfile(60)='seviribufr',     dtype(60)='seviri',    dplat(60)='m10',     dsis(60)='seviri_m10',        dval(60)=0.0,  dthin(60)=1,  dsfcalc(60)=0,
   dfile(61)='hirs4bufr',      dtype(61)='hirs4',     dplat(61)='metop-b', dsis(61)='hirs4_metop-b',     dval(61)=0.0,  dthin(61)=1,  dsfcalc(61)=0,
   dfile(62)='amsuabufr',      dtype(62)='amsua',     dplat(62)='metop-b', dsis(62)='amsua_metop-b',     dval(62)=0.0,  dthin(62)=1,  dsfcalc(62)=0,
   dfile(63)='mhsbufr',        dtype(63)='mhs',       dplat(63)='metop-b', dsis(63)='mhs_metop-b',       dval(63)=0.0,  dthin(63)=1,  dsfcalc(63)=0,
   dfile(64)='iasibufr',       dtype(64)='iasi',      dplat(64)='metop-b', dsis(64)='iasi616_metop-b',   dval(64)=0.0,  dthin(64)=1,  dsfcalc(64)=0,
   dfile(65)='gomebufr',       dtype(65)='gome',      dplat(65)='metop-b', dsis(65)='gome_metop-b',      dval(65)=0.0,  dthin(65)=2,  dsfcalc(65)=0,
   dfile(66)='atmsbufr',       dtype(66)='atms',      dplat(66)='npp',     dsis(66)='atms_npp',          dval(66)=0.0,  dthin(66)=1,  dsfcalc(66)=0,
   dfile(67)='crisbufr',       dtype(67)='cris',      dplat(67)='npp',     dsis(67)='cris_npp',          dval(67)=0.0,  dthin(67)=1,  dsfcalc(67)=0,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.false.,
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${ANAL_TIME},
   obhourset=0.,
   $SINGLEOB
 /
EOF

##################################################################################

echo " Copy GSI executable, background file, and link observation bufr to working directory"

# Save a copy of the GSI executable in the workdir
cp ${GSI_EXE} gsi.exe

# Bring over background field (it's modified by GSI so we can't link to it)
# Copy bias correction, atmospheric and surface files
cp $BK_ROOT/gdas1.t${hhg}z.abias                   ./satbias_in
cp $BK_ROOT/gdas1.t${hhg}z.satang                  ./satbias_angle

cp $BK_ROOT/gdas${resol}.t${hhg}z.bf03                    ./sfcf03
cp $BK_ROOT/gdas${resol}.t${hhg}z.bf06                    ./sfcf06
cp $BK_ROOT/gdas${resol}.t${hhg}z.bf09                    ./sfcf09

cp $BK_ROOT/gdas${resol}.t${hha}z.sgm3prep                ./sigf03
cp $BK_ROOT/gdas${resol}.t${hha}z.sgesprep                ./sigf06
cp $BK_ROOT/gdas${resol}.t${hha}z.sgp3prep                ./sigf09

# Link to the prepbufr data
ln -s ${PREPBUFR} ./prepbufr

# Link to the radiance data
suffix=tm00.bufr_d
ln -s ${OBS_ROOT}/gdas1.t00z.satwnd.${suffix}         ./satwnd
ln -s ${OBS_ROOT}/gdas1.t00z.gpsro.${suffix}         ./gpsrobufr
ln -s ${OBS_ROOT}/gdas1.t00z.spssmi.${suffix}        ./ssmirrbufr
ln -s ${OBS_ROOT}/gdas1.t00z.sptrmm.${suffix}        ./tmirrbufr
ln -s ${OBS_ROOT}/gdas1.t00z.osbuv8.${suffix}        ./sbuvbufr
ln -s ${OBS_ROOT}/gdas1.t00z.goesfv.${suffix}        ./gsnd1bufr
ln -s ${OBS_ROOT}/gdas1.t00z.1bamua.${suffix}        ./amsuabufr
ln -s ${OBS_ROOT}/gdas1.t00z.1bamub.${suffix}        ./amsubbufr
ln -s ${OBS_ROOT}/gdas1.t00z.1bhrs2.${suffix}        ./hirs2bufr
ln -s ${OBS_ROOT}/gdas1.t00z.1bhrs3.${suffix}        ./hirs3bufr
ln -s ${OBS_ROOT}/gdas1.t00z.1bhrs4.${suffix}        ./hirs4bufr
ln -s ${OBS_ROOT}/gdas1.t00z.1bmhs.${suffix}         ./mhsbufr
ln -s ${OBS_ROOT}/gdas1.t00z.1bmsu.${suffix}         ./msubufr
ln -s ${OBS_ROOT}/gdas1.t00z.airsev.${suffix}        ./airsbufr
ln -s ${OBS_ROOT}/gdas1.t00z.sevcsr.${suffix}        ./seviribufr
ln -s ${OBS_ROOT}/gdas1.t00z.mtiasi.${suffix}        ./iasibufr
ln -s ${OBS_ROOT}/gdas1.t00z.ssmit.${suffix}         ./ssmitbufr
ln -s ${OBS_ROOT}/gdas1.t00z.amsre.${suffix}         ./amsrebufr
ln -s ${OBS_ROOT}/gdas1.t00z.ssmis.${suffix}         ./ssmisbufr
ln -s ${OBS_ROOT}/gdas1.t00z.gome.${suffix}          ./gomebufr
ln -s ${OBS_ROOT}/gdas1.t00z.omi.${suffix}           ./omibufr
ln -s ${OBS_ROOT}/gdas1.t00z.mlsbufr.${suffix}        ./mlsbufr
ln -s ${OBS_ROOT}/gdas1.t00z.eshrs3.${suffix}        ./hirs3bufrears
ln -s ${OBS_ROOT}/gdas1.t00z.esamua.${suffix}        ./amsuabufrears
ln -s ${OBS_ROOT}/gdas1.t00z.esamub.${suffix}        ./amsubbufrears
ln -s ${OBS_ROOT}/gdas1.t00z.syndata.tcvitals.tm00   ./tcvitl


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

ANAVINFO=${FIX_ROOT}/global_anavinfo.l64.txt
BERROR=${FIX_ROOT}/Big_Endian/global_berror.l${LEVS}y${NLAT}.f77
SATINFO=${FIX_ROOT}/global_satinfo_reg_test.txt
scaninfo=${FIX_ROOT}/global_scaninfo.txt
SATANGL=${FIX_ROOT}/global_satangbias.txt
atmsbeamdat=${FIX_ROOT}/atms_beamwidth.txt
CONVINFO=${FIX_ROOT}/global_convinfo_reg_test.txt
OZINFO=${FIX_ROOT}/global_ozinfo.txt
PCPINFO=${FIX_ROOT}/global_pcpinfo.txt
OBERROR=${FIX_ROOT}/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=${FIX_ROOT}/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=${FIX_ROOT}/bufrtab.012

RTMFIX=${CRTM_ROOT}
RTMEMIS=${RTMFIX}/EmisCoeff/${BYTE_ORDER_CRTM}/EmisCoeff.bin
RTMAERO=${RTMFIX}/AerosolCoeff/${BYTE_ORDER_CRTM}/AerosolCoeff.bin
RTMCLDS=${RTMFIX}/CloudCoeff/${BYTE_ORDER_CRTM}/CloudCoeff.bin

#  copy Fixed fields to working directory
 cp $ANAVINFO anavinfo
 cp $BERROR   berror_stats
 cp $SATANGL  satbias_angle
 cp $atmsbeamdat  atms_beamwidth.txt
 cp $SATINFO  satinfo
 cp $scaninfo scaninfo
 cp $CONVINFO convinfo
 cp $OZINFO   ozinfo
 cp $PCPINFO  pcpinfo
 cp $OBERROR  errtable

 cp $bufrtable ./prepobs_prep.bufrtable
 cp $bftab_sst ./bftab_sstphr

#
## CRTM Spectral and Transmittance coefficients
 ln -s $RTMEMIS  EmisCoeff.bin
 ln -s $RTMAERO  AerosolCoeff.bin
 ln -s $RTMCLDS  CloudCoeff.bin
# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   cp $RTMFIX/SpcCoeff/Big_Endian/${file}.SpcCoeff.bin ./
   cp $RTMFIX/TauCoeff/Big_Endian/${file}.TauCoeff.bin ./
done

# nsatsen=`cat satinfo | wc -l`
# isatsen=1
# while [[ $isatsen -le $nsatsen ]]; do
#    flag=`head -n $isatsen satinfo | tail -1 | cut -c1-1`
#    if [[ "$flag" != "!" ]]; then
#       satsen=`head -n $isatsen satinfo | tail -1 | cut -f 2 -d" "`
#       spccoeff=${satsen}.SpcCoeff.bin
#       if  [[ ! -s $spccoeff ]]; then
#          ln -s $RTMFIX/SpcCoeff/${BYTE_ORDER_CRTM}/$spccoeff $spccoeff
#          ln -s $RTMFIX/TauCoeff/${BYTE_ORDER_CRTM}/${satsen}.TauCoeff.bin ${satsen}.TauCoeff.bin
#       fi
#    fi
#    isatsen=` expr $isatsen + 1 `
# done

#
###################################################
#  run  GSI
###################################################
echo ' Run GSI with' ${bk_core} 'background'

case $ARCH in
   'IBM_LSF')
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
      count=0
      if [[ -f pe0000.${type}_${loop} ]]; then
         count=`ls pe*${type}_${loop}* | wc -l`
      fi
      if [[ $count -gt 0 ]]; then
         cat pe*${type}_${loop}* > diag_${type}_${string}.${ANAL_TIME}
      fi
   done
done

#  Clean working directory to save only important files 
if [ ${if_clean} = clean ]; then
  echo ' Clean working directory after GSI run'
  rm -f *Coeff.bin     # all CRTM coefficient files
  rm -f pe0*           # diag files on each processor
  rm -f obs_input.*    # observation middle files
  rm -f siganl sigf03  # background middle files
  rm -f xhatsave.*     # some information on each processor
  rm -f fsize_*        # delete temperal file for bufr size
fi

exit 0

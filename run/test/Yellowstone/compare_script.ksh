#!/bin/ksh
# User defined paths
TEST_COMPILER=intel
TEST_CORES="arw nmm"
TEST_NPS="4"
TEST_DIRS="sot_2011060212 con_2011060212 gpsro_2011060212 rad_2011060212"
# User defined paths
DIFFWRF_PATH=/glade/scratch/stark/GSI/src/intel/WRF/trunk_20140814_02_3-6-1_release/external/io_netcdf/diffwrf
DATA_DIR=/glade/scratch/stark/GSI/run/${TEST_COMPILER}
NEW_PATH=${DATA_DIR}/com_trunk
BASELINE_PATH=${DATA_DIR}/ticket_444


##########################################################################

function check_paths {

 if [ ! -r "${DIFFWRF_PATH}" ]; then
     echo "ERROR: diffwrf at location '${DIFFWRF_PATH}' does not exist!"
     exit
  else
     echo ' -- diffwrf exists'
  fi

echo 'Confirm that the output paths are valid'
  if [ ! -d "${NEW_PATH}" ]; then
     echo "ERROR: Path to new output '${NEW_PATH}' does not exist!"
     exit
  else
     echo ' -- test path exists'
  fi

  if [ ! -d "${BASELINE_PATH}" ]; then
     echo "ERROR: Path to baseline output '${BASELINE_PATH}' does not exist!"
     exit
  else
     echo ' -- baseline path exists'
  fi
  echo "    First path checked ${BASELINE_PATH}"
  echo "    Second path checked ${NEW_PATH}"
  echo "    Compiler used ${TEST_COMPILER}"
  echo "    Cores tested ${TEST_CORES}"
  echo "    Processors numbers tested ${TEST_NPS}"
}

##########################################################################
function check_output_completed {
typeset TEST_PATH TEST_DIR

for TEST_PATH in ${NEW_PATH} ${BASELINE_PATH} ; do
  for TEST_DIR in ${TEST_DIRS} ; do
     if [ ! -d "${TEST_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP}" ]; then
        echo "ERROR: directory '${TEST_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP}' does not exist!"
     else
        if [ ! -r "${TEST_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP}/stdout" ]; then
           echo "ERROR: file '${TEST_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP}/stdout' does not exist!"
        else
           if [[ `grep -c 'PROGRAM GSI_ANL HAS ENDED' "${TEST_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP}/stdout" ` -eq 0 ]] ;
 then
               echo "Error: Run ${TEST_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP} failed to complete successfully"
               return
           fi
           echo "    Success: Run ${TEST_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP} completed"
        fi
     fi
  done
done

}

##########################################################################
function check_wdiff {
typeset TEST_FILE

# Confirm directories exist
  if [ ! -d "${TEST_PATH1}" ]; then
     echo "ERROR: directory '${TEST_PATH1}' does not exist!"
  else
     if [ ! -d "${TEST_PATH2}" ]; then
        echo "ERROR: directory '${TEST_PATH2}' does not exist!"
     else
# Confirm individual files exist
        for TEST_FILE in ${TEST_FILES} ; do
        echo "        --Comparing ${TEST_FILE}"
        if [ ! -r "${TEST_PATH1}/${TEST_FILE}" ]; then
           echo "ERROR: file '${TEST_PATH1}/${TEST_FILE}' does not exist!"
        else
           if [ ! -r "${TEST_PATH2}/${TEST_FILE}" ]; then
              echo "ERROR: file '${TEST_PATH2}/${TEST_FILE}' does not exist!"
           else
              diff ${TEST_PATH1}/${TEST_FILE} ${TEST_PATH2}/${TEST_FILE}
           fi
        fi
        done
     fi
  fi
}

##########################################################################
function check_wdiffwrf {
typeset TEST_FILE

# Confirm directories exist
  if [ ! -d "${TEST_PATH1}" ]; then
     echo "ERROR: directory '${TEST_PATH1}' does not exist!"
  else
     if [ ! -d "${TEST_PATH2}" ]; then
        echo "ERROR: directory '${TEST_PATH2}' does not exist!"
     else
# Confirm individual files exist
        for TEST_FILE in ${TEST_FILES} ; do
        echo "      --Comparing wrf output file ${TEST_FILE}"
        if [ ! -r "${TEST_PATH1}/${TEST_FILE}" ]; then
           echo "ERROR: file '${TEST_PATH1}/${TEST_FILE}' does not exist!"
        else
           if [ ! -r "${TEST_PATH2}/${TEST_FILE}" ]; then
              echo "ERROR: file '${TEST_PATH2}/${TEST_FILE}' does not exist!"
           else
              ${DIFFWRF_PATH} ${TEST_PATH1}/${TEST_FILE} ${TEST_PATH2}/${TEST_FILE}
           fi
        fi
        done
     fi
  fi
}

##########################################################################

##########################################################################
# Main
##########################################################################
function main {

echo 'Check to see that the top level paths are correct and that DIFFWRF can be found.'
check_paths
echo '                                                                                          '
echo 'Check to see if all the output directories exist and if the runs completed successfully.'
echo '                                                                                          '
for TEST_NP in ${TEST_NPS}; do
   for TEST_CORE in ${TEST_CORES} ; do
      check_output_completed
   done
done
echo '                                                                                          '
echo '------------------------------------------------------------------------------------------'
echo '                                                                                          '
echo 'Starting Comparison'
echo 'Comparing output from two directories.'
for TEST_NP in ${TEST_NPS} ; do
   for TEST_CORE in ${TEST_CORES} ; do
      for TEST_DIR in ${TEST_DIRS} ; do
         TEST_PATH1=${NEW_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP}
         TEST_PATH2=${BASELINE_PATH}/${TEST_DIR}_${TEST_CORE}_np${TEST_NP}
         echo '================================================================================='
         echo "   --Comparing ${TEST_DIR}_${TEST_CORE}_np${TEST_NP}"
         echo '================================================================================='
         TEST_FILES="wrf_inout"
         check_wdiffwrf
         echo '================================================================================='
         #TEST_FILES=" stdout fort.201 fort.204 fort.207 fort.203 fort.202"
         TEST_FILES=" fort.201 fort.204 fort.207 fort.203 fort.202"
#        check_wdiff
      done
   done
done

###########################################################

  exit 0
}

main


#!/bin/ksh

(( iskip = CPUS_PER_NODE / TASKS_PER_NODE ))
(( stride = iskip / OMP_NUM_THREADS ))
if [ $iskip -lt 1 ]
   then
     echo "ERROR in $0 : CPUS_PER_NODE less than TASKS_PER_NODE "
     echo " CPUS_PER_NODE = "$CPUS_PER_NODE" TASKS_PER_NODE ="$TASKS_PER_NODE
     exit
   fi
(( irat = MP_CHILD / TASKS_PER_NODE ))
(( irem = MP_CHILD - irat * TASKS_PER_NODE ))
(( cpu_id = iskip * irem ))

echo "MP_CHILD = "$MP_CHILD" cpu_id = "$cpu_id

export XLSMPOPTS="$XLSMPOPTS:startproc=$cpu_id:stride=$stride"
echo "XLSMPOPTS = $XLSMPOPTS"
$1

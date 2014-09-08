#!/bin/ksh

set -x

EXEC=test.exe

WORK=/stmpp1/George.Gayno/fov_goes
mkdir -p $WORK

cp ../sorc/$EXEC $WORK

cd $WORK

rm -f ellipse.ctl ellipse.map ellipse.dat
rm -f power.ctl power.map power.dat
rm -f config.nml

cat > config.nml << !
  &SETUP
  instr=32
  sublat=0.0
  sublon=0.0
  lat_fov = 72.0
  lon_fov = 0.0
  /
!

$EXEC > log

status=$?
if (( status != 0 ));then
  set +x
  echo ERROR IN PROGRAM
  echo EXIT WITH STATUS CODE $status
  exit $status
fi

INSTR=$(grep instr config.nml)
n=$(echo ${INSTR##*=})
if (( n == 31 )); then
  num_ch=5
elif (( n == 32 )); then
  num_ch=19
fi

cat > ellipse.ctl << !
dset ^ellipse.dat
dtype station
stnmap ^ellipse.map
options sequential
undef -999.0
title junk
tdef ${num_ch} linear jan1980 1mo
vars  1
 p  0 99 fov
endvars
!

stnmap -i ellipse.ctl

cat > power.ctl << !
dset ^power.dat
dtype station
stnmap ^power.map
options sequential
undef -999.0
title junk
tdef ${num_ch} linear jan1980 1mo
vars  1
 p  0 99 fov
endvars
!

stnmap -i power.ctl

exit 0

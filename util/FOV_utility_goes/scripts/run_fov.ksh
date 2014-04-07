#!/bin/ksh

set -x

mkdir -p /ptmpp1/George.Gayno/fov_goes
cd /ptmpp1/George.Gayno/fov_goes

rm -f ellipse.ctl ellipse.map ellipse.dat
rm -f power.ctl power.map power.dat

/global/save/George.Gayno/gsi_fov_util_goes/util/FOV_utility_goes/sorc/test.exe 

cat > ellipse.ctl << !
dset ^ellipse.dat
dtype station
stnmap ^ellipse.map
options sequential
undef -999.0
title junk
tdef 5 linear jan1980 1mo
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
tdef 5 linear jan1980 1mo
vars  1
 p  0 99 fov
endvars
!

stnmap -i power.ctl

exit 0

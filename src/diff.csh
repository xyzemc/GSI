#!/bin/csh -f
set tardir="./dr-bak"
foreach file ( *90)
echo file is $file
diff $file  $tardir/$file
end

#! /bin/sh
#

for t in t_*.py; do
   echo ---------- Test $t     ------------
   python $t
   echo ---------- End test $t ------------
   echo
done

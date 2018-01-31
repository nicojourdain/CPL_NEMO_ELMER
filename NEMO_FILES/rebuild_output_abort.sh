#!/bin/bash

NEMOdir=`grep "NEMOdir=" run_nemo_ISOMIP.sh | grep -v "#NEMOdir=" | cut -d '=' -f2 | cut -d '#' -f1`

mkdir TMPXUXU
cd TMPXUXU

rm -f rebuild_nemo.exe
ln -s ${NEMOdir}/TOOLS/REBUILD_NEMO/BLD/bin/rebuild_nemo.exe

mv ../output.abort_[0-9]???.nc .
NDOMAIN=`ls -1 output.abort_[0-9]???.nc |wc -l`

cat > nam_rebuild << EOF
  &nam_rebuild
  filebase='output.abort'
  ndomain=${NDOMAIN}
  /
EOF

cat nam_rebuild
echo " "
echo "./rebuild_nemo.exe"
./rebuild_nemo.exe

mv output.abort.nc ..
cd ..

if [ -f output.abort.nc ]; then
  rm -rf TMPXUXU
  echo " "
  echo "output.abort.nc [oK]"
else
  echo "~!@#$%^&* output.abort.nc has not been created"
fi

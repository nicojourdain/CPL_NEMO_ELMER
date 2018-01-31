#!/bin/bash

WHOAMI=`whoami`

NEMOdir=`grep "NEMOdir=" run_nemo_ISOMIP.sh | grep -v "#NEMOdir=" | cut -d '=' -f2 | cut -d '#' -f1 |sed -e "s/'//g ; s/ //g ; s/\\\`whoami\\\`/${WHOAMI}/g ; s/\"//g"`

echo "NEMOdir is $NEMOdir"

rm -rf TMPXUXU
mkdir TMPXUXU
cd TMPXUXU

rm -f rebuild_nemo.exe
ln -s -v ${NEMOdir}/TOOLS/REBUILD_NEMO/BLD/bin/rebuild_nemo.exe

mv ../mesh_mask_[0-9]???.nc .
NDOMAIN=`ls -1 mesh_mask_[0-9]???.nc |wc -l`

cat > nam_rebuild << EOF
  &nam_rebuild
  filebase='mesh_mask'
  ndomain=${NDOMAIN}
  /
EOF

cat nam_rebuild
echo " "
echo "./rebuild_nemo.exe"
./rebuild_nemo.exe

mv mesh_mask.nc ..
cd ..

if [ -f mesh_mask.nc ]; then
  rm -rf TMPXUXU
  echo " "
  echo "mesh_mask.nc [oK]"
else
  echo "~!@#$%^&* mesh_mask.nc has not been created"
fi

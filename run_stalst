#!/bin/ksh --login
#
#BSUB -oo /gpfs/hps3/emc/meso/save/Geoffrey.Manikin/nwprod2/hrrr.v3.0.0/bufr/profdat.out
#BSUB -eo /gpfs/hps3/emc/meso/save/Geoffrey.Manikin/nwprod2/hrrr.v3.0.0/bufr/profdat.err 
#BSUB -J make_profdat 
#BSUB -W 00:05
#BSUB -P HRRR-T2O
#BSUB -q "dev"
#BSUB -M 1800
#BSUB -extsched 'CRAYLINUX[]' -R '1*{select[craylinux && !vnode]} + 4*{select[craylinux && vnode]span[ptile=1] cu[type=cabinet]}' rusage[mem=1800]
#BSUB -x
#BSUB -a poe
#

module load ics
module load ibmpe
DOM=hrrr

rm fort.*

ln -sf /gpfs/hps3/emc/meso/save/Geoffrey.Manikin/staids/nam_staids.parm        fort.15
ln -sf hrrr_profdat         fort.63

echo "/gpfs/hps3/ptmp/Geoffrey.Manikin/history/wrfout_d01_2018-02-05_21_00_00" > infile
echo "netcdf" >> infile
echo "2018-02-05_21:00:00" >> infile

ln -sf infile fort.105
./staids.x < infile > staids.log 2>&1

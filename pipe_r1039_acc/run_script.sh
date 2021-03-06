#!/bin/bash --login
#PBS -N dns
#PBS -l nodes=1
#PBS -l walltime=0:20:00
#PBS -A NTI104

###export MPICH_ENV_DISPLAY=1

cd $PBS_O_WORKDIR

echo $PWD

casename=stenosis

#F_UFMTENDIAN=little
#module swap PrgEnv-cray PrgEnv-pgi
#export PGI_ACC_TIME=1

#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/cray/mpt/5.6.0.5/gni/mpich2-cray/74/lib

rm -f  $casename.sch *_host_*

echo $casename > SESSION.NAME

echo $PWD/ >> SESSION.NAME

echo $HOSTNAME > node

export MPICH_RDMA_ENABLED_CUDA=1
export CRAY_ACC_NO_ASYNC=0
export LD_LIBRARY_PATH=$CRAY_LD_LIBRARY_PATH:$LD_LIBRARY_PATH 
 
#aprun -n 4 -N1 ./nekproxy  > nek.out #.$PBS_JOBID
#aprun -n 1 -N1  ./nekproxy  > nek.out.$PBS_JOBID
#aprun -n 1 -N 1 ./nek5000+pat > pat.out.$PBS_JOBID

aprun -n 4 -N 1 ./nek5000 > nek.out.$PBS_JOBID
#aprun -n 1 -N 1 ./nek5000 > nek.out.$PBS_JOBID


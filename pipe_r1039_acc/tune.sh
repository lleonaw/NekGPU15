#!/bin/bash

rm *.sch
aprun -n1 -N1 ./nek5000 $1 $2 $3 $4 $5 $6 > Output/output.$7
rm *.sch
sleep 40
rm *.sch
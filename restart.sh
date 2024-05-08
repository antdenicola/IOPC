#!/bin/bash

### Work Directory Setting
PWD=`pwd`
echo "==================================="
echo 
echo " PARALLEL OCCAM MAKE RESTART FILES " 
echo 
echo "==================================="
echo "ver.: 1.1"
mkdir $PWD/restart
echo "Restart directory created: $PWD/restart"


### CPU Calculation from fort.15
if [ ! -e fort.15 ]; then
echo "the fort.15 does not exist !!!"
echo " Is not possible to determine the no. CPUs"
echo "Aborted !!!"
exit 0
fi

cpu=`wc -l fort.15 | awk '{print $1}' `
echo "$cpu CPUs Used for the simulation"

### Creation of Restart Input File
echo "Consistency File Check"

for i in $(seq 1 $cpu)
do
in=$[4000+i]
out=$[20+i]
if [ ! -e fort.$in ]; then
  echo "the fort.$in does not exist !!!"
  echo "Aborted !!!"
  exit 0
fi
done

for i in $(seq 1 $cpu)
 do
 in=$[4000+i]
 out=$[20+i]
 mv fort.$in $PWD/restart/fort.$out
done
echo "$cpu files processed"
echo "New input files created in: $PWD/restart"
cp fort.1 fort.3 fort.15 $PWD/restart
echo "Input files [fort.1, fort.3, fort.15] copied in: $PWD/restart"

### Removing/Compress Output Files Fort.6xxx, fort.8xxx
echo ""
echo "==================================="
echo "Output Files Managing"
echo "Options:"
echo "1 - compress output files"
echo "2 - delete output files"
read -e OP

if [ $OP -eq 1 ]; then
  echo "Compressing Output Files"
  echo "...can take time..."
  gzip fort.6*
  gzip fort.2*
  mkdir $PWD/zipped
  mv *.gz $PWD/zipped
  echo "fort.6xxx.gz and fort.2xxx.gz outputs are moved in $PWD/zipped"
elif [ $OP -eq 2 ]; then
  echo "Removing Output Files"
  rm fort.10 fort.11 fort.7
  for i in $(seq 1 $cpu)
  do
    in1=$[6000+i]
    in2=$[2000+i]
    rm fort.$in1 fort.$in2
  done
  echo "fort.6xxx and fort.2xxx are deleted !"
  echo "fort.7, fort.10, fort.11 are deleted !"
else
  echo "$OP out of range"
  exit 0
fi

echo "* * * Complete !!! * * *"

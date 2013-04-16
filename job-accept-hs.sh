#!/bin/bash

export EMAIL=frolov@chalmers.se
export PATH=/usr/local/cuda-5.0/bin:$PATH

INCOMING=/home/pfp-run/incoming
SANDBOX=$HOME/tmp
TIMEOUT=15s

if [ -e $HOME/RUNNING ]
then
   exit
fi

touch $HOME/RUNNING
mkdir $SANDBOX

scp -r pfp-queue.mkmks.org:/home/pfp-queue/incoming $INCOMING
ssh pfp-queue.mkmks.org "rm -f /home/pfp-queue/incoming/*"

while [ "`ls $INCOMING/cuda+*`" ]
do
    NEXTJOB=`ls -1 $INCOMING/cuda+* | head -n 1`
    JOBMAIL=$(echo $NEXTJOB | cut -f2 -d+)
    JOBPN=$(echo $NEXTJOB | cut -f3 -d+)
    JOBDATE=$(echo $NEXTJOB | cut -f4 -d+)

    mv "$NEXTJOB"  $SANDBOX/main.cu
    cd $SANDBOX
    nvcc main.cu -o main &>$SANDBOX/compile.log
    sudo -u nobody timeout -k 5s $TIMEOUT $1 ./main &> $SANDBOX/run.log
    if [ $? != 0 ]
    then
        echo "Timeout!" >> $SANDBOX/run.log
    fi
    if [ `stat -c%s $SANDBOX/run.log` -gt 500000 ]
    then
        echo "Nice one, now try not to flood stdout/stderr." > $SANDBOX/run.log
    fi
    echo "O HAI" | mutt -s "About that PFP job from $JOBDATE" -a $SANDBOX/compile.log -a $SANDBOX/run.log -- $JOBMAIL
done

rm -rf $SANDBOX
rm -rf $INCOMING
rm $HOME/RUNNING

sudo halt

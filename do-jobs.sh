#!/bin/sh

export AWS_ACCESS_KEY=AKIAIBJ7VS2L23MIOFPA
export AWS_SECRET_KEY=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

export INCOMING=$HOME/incoming

if [ "`ls $INCOMING/hs+*`" ]
then
    ec2-start-instances i-cdecc2ad
fi

if [ "`ls $INCOMING/cuda+*`" ]
then
    ec2-start-instances i-10d1447b
fi

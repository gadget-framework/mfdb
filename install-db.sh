#!/bin/sh

POSTPATH=/usr/lib/postgresql/9.3/bin/
SOCKPATH=$(readlink -f ./sock)
DATAPATH=$(readlink -f ./data)

[ -d $SOCKPATH ] || mkdir $SOCKPATH
[ -d $DATAPATH ] || {
    mkdir $DATAPATH
    $POSTPATH/initdb -D $DATAPATH
}

$POSTPATH/postgres -D $DATAPATH -k $SOCKPATH &
sleep 1
$POSTPATH/createuser -a -h $SOCKPATH mfdb

wait

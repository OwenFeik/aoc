#!/bin/bash

# max=0
ids=""
while read line
do
    line=$(echo "$(echo $line | sed s/[FL]/0/g)" | sed s/[BR]/1/g)

    x=0
    for (( i=0; i<7; i++ ))
    do
        x=$(($x + (${line:$i:1} << (-$i + 6))))
    done

    y=0
    for (( i=0; i<3; i++ ))
    do
        j=$(($i + 7))
        y=$(($y + (${line:$j:1} << (-$i + 2))))
    done

    id=$(( $x * 8 + $y ))

    # if [ $id -gt $max ]
    # then
    #     max=$id
    # fi

    ids=$(printf "$ids\n$id")
done

# echo $max

prev=0
printf "$ids" | sort -n | while read id
do
    if [ $(($prev + 1)) -ne $id ]
    then
        echo $(($id - 1))
    fi

    prev=$id
done 

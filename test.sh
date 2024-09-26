#!/bin/bash

echo "-- Project 3 Tests --"
for tstr in `cat $1`
do
    tst=`echo $tstr | sed 's/\([^:]*\):.*/\1/g'`
    r=`echo $tstr | sed 's/[^:]*:\(.*\)/\1/g'`
    echo -n $tst
    echo -ne "\t"
    progrr=`./main -nossa -interpllvm $tst 2> /dev/null` > /dev/null
    progret=$?
    progr=`echo $progrr | sed 's/Result: \(.*\)/\1/g'`
    if [[ ${r:0:1} = "E" ]]
    then
	rv=${r:1:2}
	if [[ $progret -eq $rv ]]
	then
	    echo -ne '\033[0;32mPASSED\033[0m'
	else
	    echo -ne "\033[0;31mFAILED\033[0m: Expected: Error"
	fi
    else
	if [[ $r = $progr ]]
	then
	    echo -ne '\033[0;32mPASSED\033[0m'
	else
	    echo -ne "\033[0;31mFAILED\033[0m: Expected: $r, Got: $progr"
	fi
    fi
    echo ""
done
    
    
    

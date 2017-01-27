#!/bin/bash

if [[ $# < 2 ]]
then
    echo "You need to input a file name and the number of patients per file"
    echo "$0 filename.xml maxpatients"
    exit 1
fi

echo $1 $2

# change the end subject for something different - <tataa>
sed -e 's|</subject>|<tataaa>|' $1 > $1.tmp

# Break the file into chuncks where <d:subject> occurs.
awk '/\<subject\>/ { delim++ } {file = sprintf("chunks_%s.txt", int(delim/'$2')); print >> file; }' $1.tmp

firstline=$(head -n1 chunks_0.txt)
lastline="</data></context></document>"
nfiles=$(expr `ls chunks_* | wc -l` - 1)

# loop over all the files to add header and footer for each file that needs it
for i in $(seq 0 ${nfiles})
do
    echo chunks_${i}

    sed -i 's|<tataaa>|</subject>|' chunks_${i}.txt

    if [ $i -lt ${nfiles} ]
    then
        echo "$lastline" >> chunks_${i}.txt
    fi


    if [ $i -gt 0 ]
    then
        sed -i '1s|^|'"$firstline"'|' chunks_${i}.txt
    fi

done

#rm $1.tmp


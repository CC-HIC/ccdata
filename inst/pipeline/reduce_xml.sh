#!/bin/bash
# example:
# ./reduce_xml anon_CC.xml 10

if [ -e output.xml ]
then
  rm output.xml
fi

patient_num=$(($2-1))

touch output.xml
count=0

while read line
do
  if [ $count -gt $patient_num ]
  then
    echo 'end'
    break
  else
    echo $line >> output.xml
    if echo $line|grep -q '<\/d:subject>'
    then
      count=$(($count+1))
    fi
  fi
done < $1

tail -n 1 $1 >> output.xml

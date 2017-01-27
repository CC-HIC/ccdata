#!/bin/bash 

if [[ $(sed --help 2>&1 | grep GNU) ]]; then
  sed_i () { sed -i "$@"; }
  gnused=1
else
  sed_i () { sed -i '' "$@"; }
  gnused=0
fi

if [[ $# < 2 ]]
then
    echo "You need to input a file name and the number of patients per file"
    echo "$0 filename.xml maxpatients"
    exit 1
fi

default_ext='partxml'
# <d:xx> or <xx> file?
dchar="d:"
dchar_exist=$(head -2 $1 | grep -c "<${dchar}")
if [ ${dchar_exist} -eq 0 ]
then
    dchar=""
fi

subject="${dchar}subject"

# Add lines before and after each subject starts
sed -e  's|<'"${subject}"'>|\n<'"${subject}"'>\n|' ${1} > ${1}.tmp

# change the end subject for something different - <cut_here>
# so it is not counted in the awk below.
if [[ $gnused == 1 ]]; then
  echo "gnu here"
  sed -e 's|</'"${subject}"'>|<cut_here>\n|' ${1} > ${1}.tmp
else
  sed -e 's|</'"${subject}"'>|<cut_here>\'$'\n|' ${1} > ${1}.tmp
fi

if [[ -e ${1}_0.${default_ext} ]]; then
  rm ${1}*${default_ext}
fi
# Break the file into chunks where <${subject}> occurs.
# Each time <subject> is found, delim will increase
#   if delim/maxpatients (2nd argument) == 1 then
#   create a new file.
# initialising delim as -1 so the first file includes the
# number of subjects asked.
awk 'BEGIN {delim=-1} \
         /\<'"${subject}"'\>/ { delim++ } \
                  {file = sprintf("'${1}'_%s.'${default_ext}'", int(delim/'${2}'));\
                   print >> file; } \
     END { print "'${1}' has ", delim+1, "subjects"}' ${1}.tmp


# extract the header of the file with its meta.
# - extract everything till the first <subject> (what's used to separate the file)
# - remove the instance for <subject> so it's not repeated when inserted.
# - remove all no printing characters - it seems there's one making the insertion to
#   fail afterwards.
# head won't work because some files run over multiple lines
# firstlines=$(sed -n '1,/<'"${subject}"'>/p' ${1}_0.${default_ext} | \
#                     sed 's/<'"${subject}"'>//' | tr -dc '[:print:]')
firstlines=$(sed -n 's/\(.*\)<'"${subject}"'>/\1/p' ${1}_0.${default_ext} | tr -dc '[:print:]')

lastline="</${dchar}data></${dchar}context></${dchar}document>"
nfiles=$(ls "${1}"_* | wc -l)

# loop over all the files to add header and footer for each file that needs it

for ((i=0; i<${nfiles}; i++))
do
    output=${1}_${i}.${default_ext}
    # replace the label changed before
    sed -i  -e "s|<cut_here>|</${subject}>|" ${output}

    # add footer to the files
    if [ $i -lt $((${nfiles} - 1)) ]
    then
        echo "$lastline" >> ${output}
    fi

    # add header to the files
    if [ $i -gt 0 ]
    then
        sed -i "1s|^|${firstlines}|" ${output}
    fi
done

# Remove the temporary file used
rm ${1}.tmp

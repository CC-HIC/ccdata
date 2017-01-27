#!/bin/bash

# Pipeline code to generate the ccdata structure


DEFAULT_SPACES='xml'
allPatients='all_patients.RData'
allPatients_untime='delta_num.RData'

function remove_spaces (){
    mv "${1}" "${1// /_}"
}
# make the function available as a command so find can use it.
export -f remove_spaces

function remove_spaces_ext (){
    if [ -z "$1" ]
    then
        echo "-- removing spaces on \"${DEFAULT_SPACES}\" files -- "
    else
        echo "-- removing spaces on \"${1}\" files -- "
    fi

    extension=${1-$DEFAULT_SPACES}

    FILES_spaced_num=$(find -L ./ -type f -iname '* *.'"${extension}" | wc -l)
    find -L ./ -type f -iname '*\ *.'"${extension}" -exec bash -c 'remove_spaces "{}"' \;
    # if you try to do this as a loop read:
    #  http://mywiki.wooledge.org/BashPitfalls#for_i_in_.24.28ls_.2A.mp3.29

    echo "-- converted ${FILES_spaced_num} files --"
}


#============================================================
# remove spaces from file names, otherwise xargs won't parallelise
#============================================================
remove_spaces_ext

#============================================================
# Break files into smaller chunks in parallel
#  - find the files, sort them by size (%k)
#  - extract the file names and run break_into 4 at a time
#  => filename.xml_xx.xml; where xx is a non padded number
#============================================================
find -L ./ -type f -iname '*.xml' -printf "%k %p\n" | sort -nr \
    | awk '{print $2}' | xargs -n1 -P 4 -I % ./break_into.sh % 3

#============================================================
# Convert each portion to ccdata
#============================================================
find -L ./ -type f -iname '*.partxml' | xargs -n1 -P 4 ./extract_data.r

#============================================================
# Combine all the files
#============================================================
./combine_data.r ${allPatients}

#============================================================
# Anonymise data removing timestamp
#============================================================
./untimeit.r ${allPatients} ${allPatients_untime}

echo "Files ${allPatients} and ${allPatients_untime} created."

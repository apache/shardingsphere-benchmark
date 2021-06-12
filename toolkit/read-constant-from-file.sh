#!/bin/bash

FILE_NAME=$1
CONSTANT_NAME=$2

if [ -z "${FILE_NAME}" ]; then
    echo "file name is empty"
    exit
fi

if [ ! -f "${FILE_NAME}" ]; then
    echo "file ${FILE_NAME} doesn't exist"
fi

if [ -z "${CONSTANT_NAME}" ]; then
    echo "constant name is empty"
    exit
fi

while read -r line
do
 KEY=$(echo "$line" | cut -f1 -d"=")
 if [ "${CONSTANT_NAME}" = "${KEY}" ]; then
     echo "$line" | cut -f2 -d"="
 fi

done < "${FILE_NAME}"


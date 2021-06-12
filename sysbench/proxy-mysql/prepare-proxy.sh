#!/bin/bash

DATABASE_TYPE=$(sh toolkit/read-constant-from-file.sh .env "PROXY_MYSQL")
BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
PREPARED_CONF_PATH=$(sh toolkit/read-constant-from-file.sh .env "PREPARED_CONF_PATH")
PROXY_DIRECTORY_NAME=$(sh toolkit/read-constant-from-file.sh .env "PROXY_DIRECTORY_NAME")
MYSQL_DRIVER=$(sh toolkit/read-constant-from-file.sh .env "MYSQL_DRIVER")

# TODO this action is useless
if [ ! -d "${BASE_PATH}/${DATABASE_TYPE}/${PREPARED_CONF_PATH}" ]; then
  cp -R sysbench/${DATABASE_TYPE}/${PREPARED_CONF_PATH} ${BASE_PATH}/${DATABASE_TYPE}/
fi

# debug info
CURRENTPATH=`pwd`
echo "current path is ${CURRENTPATH}"
if [ ! -f ${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}/lib/${MYSQL_DRIVER} ]; then
  cp sysbench/${DATABASE_TYPE}/${PREPARED_CONF_PATH}/${MYSQL_DRIVER} ${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}/lib/
fi



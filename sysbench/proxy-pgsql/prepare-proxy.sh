#!/bin/bash

DATABASE_TYPE=$(sh toolkit/read-constant-from-file.sh .env "PROXY_PGSQL")
BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
PREPARED_CONF_PATH=$(sh toolkit/read-constant-from-file.sh .env "PREPARED_CONF_PATH")
PROXY_DIRECTORY_NAME=$(sh toolkit/read-constant-from-file.sh .env "PROXY_DIRECTORY_NAME")

if [ ! -d "${BASE_PATH}/${DATABASE_TYPE}/${PREPARED_CONF_PATH}" ]; then
  cp -R sysbench/${DATABASE_TYPE}/${PREPARED_CONF_PATH} ${BASE_PATH}/${DATABASE_TYPE}/
fi



#!/bin/bash

BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
DATABASE_TYPE=$(sh toolkit/read-constant-from-file.sh .env "PROXY_PGSQL")
PREPARED_CONF=$(sh toolkit/read-constant-from-file.sh .env "PREPARED_CONF")
PROXY_DIRECTORY_NAME=$(sh toolkit/read-constant-from-file.sh .env "PROXY_DIRECTORY_NAME")
PROXY_START_BASH_FILE=$(sh toolkit/read-constant-from-file.sh .env "PROXY_START_BASH_FILE")
TEST_FUNCTION=$1

cd ${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}

DEPLOY_PATH=`pwd`
PIDS=`ps -ef | grep java | grep proxy | grep "${DEPLOY_PATH}" | grep -v grep |awk '{print $2}'`
if [ -n "$PIDS" ]; then
    kill -9 $PIDS
fi

rm -rf ${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}/conf/*.yaml
cp ${BASE_PATH}/${DATABASE_TYPE}/${PREPARED_CONF}/server.yaml ${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}/conf/
cp ${BASE_PATH}/${DATABASE_TYPE}/${PREPARED_CONF}/${TEST_FUNCTION}/config-${TEST_FUNCTION}.yaml ${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}/conf/
sudo sh ${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}/bin/${PROXY_START_BASH_FILE}


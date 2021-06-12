#!/bin/bash

CURRENT_FILE_PATH=$(cd `dirname $0` ; pwd)
CURRENT_PATH=$(pwd)

cd "${CURRENT_FILE_PATH}/../../"

BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
DATABASE_TYPE=$(sh toolkit/read-constant-from-file.sh .env "PROXY_MYSQL")
PROXY_DIRECTORY_NAME=$(sh toolkit/read-constant-from-file.sh .env "PROXY_DIRECTORY_NAME")
PROXY_STOP_BASH_FILE=$(sh toolkit/read-constant-from-file.sh .env "PROXY_STOP_BASH_FILE")
PROXY_TAR_NAME=$(sh toolkit/read-constant-from-file.sh .env "PROXY_TAR_NAME")
SHARDINGSPHERE_PROJECT_NAME=$(sh toolkit/read-constant-from-file.sh .env "SHARDINGSPHERE_PROJECT_NAME")
GIT_MIRROR=$(sh toolkit/read-constant-from-file.sh .env "GIT_MIRROR")

# 1. create proxy MySQL directory
echo "start to mkdir ${BASE_PATH}/${DATABASE_TYPE}"
if [ ! -d "${BASE_PATH}/${DATABASE_TYPE}" ]; then
    mkdir -p "${BASE_PATH}/${DATABASE_TYPE}"
fi

# 2. stop & clean proxy
echo "start to clean proxy"
cd "${BASE_PATH}/${DATABASE_TYPE}"
PROXY_DIRECTORIES_COUNT=$(ls "${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}" 2> /dev/null | wc -l)
if [ "${PROXY_DIRECTORIES_COUNT}" != 0 ]; then
  DEPLOY_PATH="${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}"
  PIDS=`ps -ef | grep java | grep proxy | grep "${DEPLOY_PATH}" | grep -v grep |awk '{print $2}'`
  if [ -n "$PIDS" ]; then
    echo "stop the previous proxy ......"
    kill $PIDS
  fi
fi

# 3. git clone shardingsphere
if [ -d "${BASE_PATH}/${DATABASE_TYPE}/${SHARDINGSPHERE_PROJECT_NAME}" ]; then
    cd "${BASE_PATH}/${DATABASE_TYPE}/${SHARDINGSPHERE_PROJECT_NAME}"
    git pull
else
  rm -rf ${SHARDINGSPHERE_PROJECT_NAME}

  # in this situation, choose a faster mirror and then change to original github mirror
  echo "start to clone shardingsphere"
  git clone "${GIT_MIRROR}" "${BASE_PATH}/${DATABASE_TYPE}/${SHARDINGSPHERE_PROJECT_NAME}"
  cd "${BASE_PATH}/${DATABASE_TYPE}/${SHARDINGSPHERE_PROJECT_NAME}"
  git remote set-url --push origin https://github.com/apache/shardingsphere.git
  git remote set-url origin https://github.com/apache/shardingsphere.git
  git pull
fi

rm -rf ${PROXY_DIRECTORY_NAME}

# 4. install proxy
echo "start to maven install and extract the distribution"
cd "${BASE_PATH}/${DATABASE_TYPE}/${SHARDINGSPHERE_PROJECT_NAME}"
./mvnw clean install -Dmaven.javadoc.skip=true -B -Drat.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -Prelease
cp "${BASE_PATH}/${DATABASE_TYPE}/${SHARDINGSPHERE_PROJECT_NAME}/shardingsphere-distribution/shardingsphere-proxy-distribution/target/"${PROXY_TAR_NAME} "${BASE_PATH}/${DATABASE_TYPE}/"
cd "${BASE_PATH}/${DATABASE_TYPE}"
tar -zxvf ${PROXY_TAR_NAME}
rm -rf ${PROXY_TAR_NAME}
rm -rf ${BASE_PATH}/${DATABASE_TYPE}/${PROXY_DIRECTORY_NAME}/conf/*.yaml


cd "${CURRENT_PATH}"

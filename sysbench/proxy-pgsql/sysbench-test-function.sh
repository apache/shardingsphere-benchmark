#!/bin/sh

BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
DATABASE_TYPE=$(sh toolkit/read-constant-from-file.sh .env "PROXY_PGSQL")
SYSBENCH_RESULT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_RESULT")
BUILD_NUMBER_FILE=$(sh toolkit/read-constant-from-file.sh .env "BUILD_NUMBER_FILE")
SYSBENCH_PGSQL_SCRIPT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_PROXY_PGSQL_SCRIPT")
TEST_FUNCTION=$1


BUILD_NUMBER_FILE_PATH="${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER_FILE}"

# debug
echo "ctouch path is : ${BUILD_NUMBER_FILE_PATH}"

if [ ! -f "${BUILD_NUMBER_FILE_PATH}" ]; then
  mkdir -p ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/
  touch ${BUILD_NUMBER_FILE_PATH}
fi

BUILD_NUMBER=$(cat "${BUILD_NUMBER_FILE_PATH}" | awk 'END {print}')

## if there is no build number, then create the first folder
if [ ! -n "${BUILD_NUMBER}" ]; then
  BUILD_NUMBER=0
fi

let BUILD_NUMBER+=1;

## if there is a failure test, then delete the folder
if [ -d "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER}"  ]; then
  # if path is empty, ":?" will stop the rm command delete everything in system.
  rm -rf "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER}:?"
fi

mkdir -p "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER}"
cp ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_PGSQL_SCRIPT} ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER}/${SYSBENCH_PGSQL_SCRIPT}
cd ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER}

#Debug
echo "path : ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER}"
echo "python file path : ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}"

echo "the execution script : ${SYSBENCH_PGSQL_SCRIPT}"
sh ${SYSBENCH_PGSQL_SCRIPT}

rm -rf ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER}/${SYSBENCH_PGSQL_SCRIPT}
cd ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}
ls -v | tail -n14 > ${BUILD_NUMBER_FILE_PATH}


cd ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}
python3 plot_graph.py ${TEST_FUNCTION}


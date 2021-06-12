#!/bin/sh

BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
DATABASE_TYPE=$(sh toolkit/read-constant-from-file.sh .env "OPENGAUSS")
SYSBENCH_RESULT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_RESULT")
BUILD_NUMBER_FILE=$(sh toolkit/read-constant-from-file.sh .env "BUILD_NUMBER_FILE")
SYSBENCH_OPENGAUSS_SCRIPT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_OPENGAUSS_SCRIPT")
BUILD_NUMBER_FILE_PATH="${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/${BUILD_NUMBER_FILE}"

if [ ! -f "${BUILD_NUMBER_FILE_PATH}" ]; then
  mkdir -p ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/
  touch ${BUILD_NUMBER_FILE_PATH}
fi

# debug
echo "build number file path is : ${BUILD_NUMBER_FILE_PATH}"

BUILD_NUMBER=$(cat "${BUILD_NUMBER_FILE_PATH}" | awk 'END {print}')
# debug info
echo "build number is : ${BUILD_NUMBER}"

## if there is no build number, then create the first folder
if [ ! -n "${BUILD_NUMBER}" ]; then
  BUILD_NUMBER=0
fi

let BUILD_NUMBER+=1;

#debug info
echo "now build number is : ${BUILD_NUMBER}"

## if there is a failure test, then delete the folder
if [ -d "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/${BUILD_NUMBER}"  ]; then
  rm -rf "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/${BUILD_NUMBER}"
fi

mkdir -p "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/${BUILD_NUMBER}"
#debug info
echo "create the number folder : ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/${BUILD_NUMBER}"

cp ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_OPENGAUSS_SCRIPT} ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/${BUILD_NUMBER}/${SYSBENCH_OPENGAUSS_SCRIPT}
cd ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/${BUILD_NUMBER}

#Debug
echo "path : ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/${BUILD_NUMBER}"
echo "the execution script : ${SYSBENCH_OPENGAUSS_SCRIPT}"
sh ${SYSBENCH_OPENGAUSS_SCRIPT}

rm -rf ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${TEST_FUNCTION}/${BUILD_NUMBER}/${SYSBENCH_OPENGAUSS_SCRIPT}
cd ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}
ls -v | tail -n14 > ${BUILD_NUMBER_FILE_PATH}


cd ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}
python3 plot_graph.py opengauss


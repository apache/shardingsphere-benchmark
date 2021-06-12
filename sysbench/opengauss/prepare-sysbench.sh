#!/bin/bash

BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
SYSBENCH_RESULT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_RESULT")
SYSBENCH_GRAPH=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_GRAPH")
SYSBENCH_GRAPH_PYTHON_FILE=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_GRAPH_PYTHON_FILE")
SYSBENCH_OPENGAUSS_SCRIPT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_OPENGAUSS_SCRIPT")
SYSBENCH_TEST_FUNCTION=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_TEST_FUNCTION")
DATABASE_TYPE=$(sh toolkit/read-constant-from-file.sh .env "OPENGAUSS")
FOLDER_COUNT=$(sh toolkit/read-constant-from-file.sh .env "FOLDER_COUNT")
echo "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}"

if [ ! -d "${BASE_PATH}/opengauss/${SYSBENCH_RESULT}" ]; then
  # debug info
  echo "start to mkdir the sysbench related directories"
  mkdir -p ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}
  mkdir -p ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH}
  mkdir -p ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}
  mkdir ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${DATABASE_TYPE}/{1..15}
  mkdir -p ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH}/${DATABASE_TYPE}
  cp toolkit/${SYSBENCH_GRAPH_PYTHON_FILE} ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH_PYTHON_FILE}
fi

if [ ! -f "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_OPENGAUSS_SCRIPT}" ]; then
  cp sysbench/${DATABASE_TYPE}/${SYSBENCH_OPENGAUSS_SCRIPT} ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_OPENGAUSS_SCRIPT}
  cp sysbench/${DATABASE_TYPE}/${SYSBENCH_TEST_FUNCTION} ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_TEST_FUNCTION}
fi

chmod +x ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_TEST_FUNCTION}
chmod +x ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_OPENGAUSS_SCRIPT}
chmod +x ${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH_PYTHON_FILE}
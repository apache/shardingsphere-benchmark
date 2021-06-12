#!/bin/bash

BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
SYSBENCH_RESULT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_RESULT")
SYSBENCH_GRAPH=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_GRAPH")
SYSBENCH_GRAPH_PYTHON_FILE=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_GRAPH_PYTHON_FILE")
SYSBENCH_PURE_MYSQL_SCRIPT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_PURE_MYSQL_SCRIPT")
SYSBENCH_TEST_FUNCTION=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_TEST_FUNCTION")
DATABASE_TYPE=""$(sh toolkit/read-constant-from-file.sh .env "MYSQL")""
FOLDER_COUNT=$(sh toolkit/read-constant-from-file.sh .env "FOLDER_COUNT")
echo "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}"

if [ ! -d "${BASE_PATH}/pure-mysql/${SYSBENCH_RESULT}" ]; then
  # debug info
  echo "start to mkdir the sysbench related directories"
  mkdir -p ${BASE_PATH}/pure-mysql/${SYSBENCH_RESULT}
  mkdir -p ${BASE_PATH}/pure-mysql/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH}
  mkdir -p ${BASE_PATH}/pure-mysql/${SYSBENCH_RESULT}/${DATABASE_TYPE}
  mkdir ${BASE_PATH}/pure-mysql/${SYSBENCH_RESULT}/${DATABASE_TYPE}/{1..15}
  mkdir -p ${BASE_PATH}/pure-mysql/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH}/${DATABASE_TYPE}
  cp toolkit/${SYSBENCH_GRAPH_PYTHON_FILE} ${BASE_PATH}/pure-mysql/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH_PYTHON_FILE}
fi

# todo , change pure mysql to mysql.
if [ ! -f "${BASE_PATH}/pure-mysql/${SYSBENCH_MYSQL_SCRIPT}" ]; then
  cp sysbench/mysql/${SYSBENCH_PURE_MYSQL_SCRIPT} ${BASE_PATH}/pure-mysql/${SYSBENCH_PURE_MYSQL_SCRIPT}
  cp sysbench/mysql/${SYSBENCH_TEST_FUNCTION} ${BASE_PATH}/pure-mysql/${SYSBENCH_TEST_FUNCTION}
fi

chmod +x ${BASE_PATH}/pure-mysql/${SYSBENCH_TEST_FUNCTION}
chmod +x ${BASE_PATH}/pure-mysql/${SYSBENCH_PURE_MYSQL_SCRIPT}
chmod +x ${BASE_PATH}/pure-mysql/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH_PYTHON_FILE}
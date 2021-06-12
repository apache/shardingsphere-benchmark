#!/bin/bash

BASE_PATH=$(sh toolkit/read-constant-from-file.sh .env "BASE_PATH")
SYSBENCH_RESULT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_RESULT")
SYSBENCH_GRAPH=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_GRAPH")
SYSBENCH_GRAPH_PYTHON_FILE=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_GRAPH_PYTHON_FILE")
SYSBENCH_PURE_PGSQL_SCRIPT=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_PURE_PGSQL_SCRIPT")
SYSBENCH_TEST_FUNCTION=$(sh toolkit/read-constant-from-file.sh .env "SYSBENCH_TEST_FUNCTION")
DATABASE_TYPE=""$(sh toolkit/read-constant-from-file.sh .env "PGSQL")""
FOLDER_COUNT=$(sh toolkit/read-constant-from-file.sh .env "FOLDER_COUNT")
echo "${BASE_PATH}/${DATABASE_TYPE}/${SYSBENCH_RESULT}"

if [ ! -d "${BASE_PATH}/pure-pgsql/${SYSBENCH_RESULT}" ]; then
  # debug info
  echo "start to mkdir the sysbench related directories"
  mkdir -p ${BASE_PATH}/pure-pgsql/${SYSBENCH_RESULT}
  mkdir -p ${BASE_PATH}/pure-pgsql/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH}
  mkdir -p ${BASE_PATH}/pure-pgsql/${SYSBENCH_RESULT}/${DATABASE_TYPE}
  mkdir ${BASE_PATH}/pure-pgsql/${SYSBENCH_RESULT}/${DATABASE_TYPE}/{1..15}
  mkdir -p ${BASE_PATH}/pure-pgsql/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH}/${DATABASE_TYPE}
  cp toolkit/${SYSBENCH_GRAPH_PYTHON_FILE} ${BASE_PATH}/pure-pgsql/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH_PYTHON_FILE}
fi

# todo , change pure pgsql to pgsql.
if [ ! -f "${BASE_PATH}/pure-pgsql/${SYSBENCH_PURE_PGSQL_SCRIPT}" ]; then
  cp sysbench/pgsql/${SYSBENCH_PURE_PGSQL_SCRIPT} ${BASE_PATH}/pure-pgsql/${SYSBENCH_PURE_PGSQL_SCRIPT}
  cp sysbench/pgsql/${SYSBENCH_TEST_FUNCTION} ${BASE_PATH}/pure-pgsql/${SYSBENCH_TEST_FUNCTION}
fi

chmod +x ${BASE_PATH}/pure-pgsql/${SYSBENCH_TEST_FUNCTION}
chmod +x ${BASE_PATH}/pure-pgsql/${SYSBENCH_PURE_PGSQL_SCRIPT}
chmod +x ${BASE_PATH}/pure-pgsql/${SYSBENCH_RESULT}/${SYSBENCH_GRAPH_PYTHON_FILE}
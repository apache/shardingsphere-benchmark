#!/bin/sh

ENV_FILE_PATH=$1

DATABASE_TYPE=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "DATABASE_TYPE")
DATABASE_HOST=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "DATABASE_HOST")
DATABASE_PORT=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "DATABASE_PORT")
DATABASE_USER_NAME=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "DATABASE_USER_NAME")
DATABASE_PASSWORD=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "DATABASE_PASSWORD")
DATABASE_NAME=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "DATABASE_NAME")
TABLES=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "TABLES")
TABLE_SIZE=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "TABLE_SIZE")
REPORT_INTERVAL=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "REPORT_INTERVAL")
TIME=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "TIME")
THREADS=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "THREADS")
MAX_REQUESTS=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "MAX_REQUESTS")
PERCENTILE=$(sh toolkit/read-constant-from-file.sh '${ENV_FILE_PATH}' "PERCENTILE")

sysbench oltp_read_only --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=${REPORT_INTERVAL} --time=120 --threads=32 --max-requests=0 --percentile=99 --rand-type=uniform --range_selects=off --auto_inc=off cleanup

sysbench oltp_read_only --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=${REPORT_INTERVAL} --time=360 --threads=32 --max-requests=0 --percentile=99 --rand-type=uniform --range_selects=off --auto_inc=off prepare


sysbench oltp_read_only        --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=30  --time=180 --threads=32 --max-requests=0 --percentile=99  --range_selects=off --rand-type=uniform --auto_inc=off run

sysbench oltp_read_only        --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=30  --time=180 --threads=32 --max-requests=0 --percentile=99  --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_read_only.master.txt

sysbench oltp_point_select     --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=30  --time=180 --threads=32 --max-requests=0 --percentile=99  --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_point_select.master.txt

sysbench oltp_read_write        --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=30  --time=180 --threads=32 --max-requests=0 --percentile=99  --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_readwrite.master.txt

sysbench oltp_write_only       --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=30  --time=180 --threads=32 --max-requests=0 --percentile=99  --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_write_only.master.txt

sysbench oltp_update_index     --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=30  --time=180 --threads=32 --max-requests=0 --percentile=99  --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_update_index.master.txt

sysbench oltp_update_non_index --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=30  --time=180 --threads=32 --max-requests=0 --percentile=99  --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_update_non_index.master.txt

sysbench oltp_delete           --${DATABSE_TYPE}-host=${DATABASE_HOST} --${DATABASE_TYPE}-port=${DATABASE_PORT} --${DATABASE_TYPE}-user=${DATABASE_USER_NAME} --${DATABASE_TYPE}-password=${DATABASE_PASSWORD} --${DATABSE_TYPE}-db=${DATABASE_NAME} --tables=${TABLES} --table-size=${TABLE_SIZE} --report-interval=30  --time=180 --threads=32 --max-requests=0 --percentile=99  --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_delete.master.txt
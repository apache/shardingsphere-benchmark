PROXY_HOST=10.12.3.160

sysbench oltp_read_only --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=10 --time=120 --threads=10 --max-requests=0 --percentile=99 --rand-type=uniform --range_selects=off --auto_inc=off cleanup

sysbench oltp_read_only --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=10 --time=360 --threads=10 --max-requests=0 --percentile=99 --rand-type=uniform --range_selects=off --auto_inc=off prepare


sysbench oltp_read_only         --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=30  --time=15 --threads=16 --max-requests=0 --percentile=99 --range_selects=off --rand-type=uniform --auto_inc=off run

sysbench oltp_read_only        --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=30  --time=15 --threads=16 --max-requests=0 --percentile=99 --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_read_only.master.txt

sysbench oltp_point_select     --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=30  --time=15 --threads=16 --max-requests=0 --percentile=99 --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_point_select.master.txt

sysbench oltp_read_write        --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=60  --time=21600 --threads=16 --max-requests=0 --percentile=99 --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_readwrite.master.txt

sysbench oltp_write_only       --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=60  --time=21600 --threads=16 --max-requests=0 --percentile=99 --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_write_only.master.txt

sysbench oltp_update_index     --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=30  --time=15 --threads=16 --max-requests=0 --percentile=99 --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_update_index.master.txt

sysbench oltp_update_non_index --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=30  --time=15 --threads=16 --max-requests=0 --percentile=99 --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_update_non_index.master.txt

sysbench oltp_delete           --db-driver=pgsql --pgsql-host=${PROXY_HOST} --pgsql-port=3307 --pgsql-user=root --pgsql-password='root' --pgsql-db=sbtest --tables=10 --table-size=100 --report-interval=30  --time=15 --threads=16 --max-requests=0 --percentile=99 --range_selects=off --rand-type=uniform --auto_inc=off run | tee oltp_delete.master.txt

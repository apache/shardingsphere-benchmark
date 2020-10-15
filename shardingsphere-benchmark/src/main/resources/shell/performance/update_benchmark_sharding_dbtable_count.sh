let db_count=$1-1
let table_count=$2-1
let db_count_smallshards=$3-1
let table_count_smallshards=$4-1
benchmark_resource_dir="/export/benchmark/shardingsphere-benchmark/src/main/resources"
chmod -R 777 $benchmark_resource_dir
cd $benchmark_resource_dir

sed -i "s/{0..[1-9]*}.sbtest\${\0..[1-9]*}/{0..$db_count}.sbtest\${0..$table_count}/g" `grep '{0..[1-9]*}.sbtest\${0..[1-9]*}' -rl ./yaml/fullrouting`
sed -i "s/{id % [1-9]*}/{id % $1}/g" `grep '{id % [1-9]*}' -rl ./yaml/fullrouting`
sed -i "s/{k % [0-9]*}/{k % $2}/g" `grep '{k % [0-9]*}' -rl ./yaml/fullrouting`
sed -i "s/minPoolSize: [0-9]*}/minPoolSize: $3}/g" `grep 'minPoolSize: [0-9]*' -rl ./yaml/fullrouting`
sed -i "s/maxPoolSize: [0-9]*}/maxPoolSize: $4}/g" `grep 'maxPoolSize: [0-9]*' -rl ./yaml/fullrouting`
sed -i "s/maximumPoolSize: [0-9]*}/maximumPoolSize: $4}/g" `grep 'maximumPoolSize: [0-9]*' -rl ./yaml/fullrouting`
sed -i "s/max.connections.size.per.query: [0-9]*}/max.connections.size.per.query: $5}/g" `grep 'max.connections.size.per.query: [0-9]*' -rl ./yaml/fullrouting`

sed -i "s/{0..[1-9]*}.sbtest\${\0..[1-9]*}/{0..$db_count}.sbtest\${0..$table_count}/g" `grep '{0..[1-9]*}.sbtest\${0..[1-9]*}' -rl ./yaml/rangerouting`
sed -i "s/{id % [1-9]*}/{id % $1}/g" `grep '{id % [1-9]*}' -rl ./yaml/rangerouting`
sed -i "s/{k % [0-9]*}/{k % $2}/g" `grep '{k % [0-9]*}' -rl ./yaml/rangerouting`
sed -i "s/minPoolSize: [0-9]*}/minPoolSize: $3}/g" `grep 'minPoolSize: [0-9]*' -rl ./yaml/rangerouting`
sed -i "s/maxPoolSize: [0-9]*}/maxPoolSize: $4}/g" `grep 'maxPoolSize: [0-9]*' -rl ./yaml/rangerouting`
sed -i "s/maximumPoolSize: [0-9]*}/maximumPoolSize: $4}/g" `grep 'maximumPoolSize: [0-9]*' -rl ./yaml/rangerouting`
sed -i "s/max.connections.size.per.query: [0-9]*}/max.connections.size.per.query: $5}/g" `grep 'max.connections.size.per.query: [0-9]*' -rl ./yaml/rangerouting`

sed -i "s/{0..[1-9]*}.sbtest\${\0..[1-9]*}/{0..$db_count}.sbtest\${0..$table_count}/g" `grep '{0..[1-9]*}.sbtest\${0..[1-9]*}' -rl ./yaml/singlerouting`
sed -i "s/{id % [1-9]*}/{id % $1}/g" `grep '{id % [1-9]*}' -rl ./yaml/singlerouting`
sed -i "s/{k % [0-9]*}/{k % $2}/g" `grep '{k % [0-9]*}' -rl ./yaml/singlerouting`
sed -i "s/minPoolSize: [0-9]*}/minPoolSize: $3}/g" `grep 'minPoolSize: [0-9]*' -rl ./yaml/singlerouting`
sed -i "s/maxPoolSize: [0-9]*}/maxPoolSize: $4}/g" `grep 'maxPoolSize: [0-9]*' -rl ./yaml/singlerouting`
sed -i "s/maximumPoolSize: [0-9]*}/maximumPoolSize: $4}/g" `grep 'maximumPoolSize: [0-9]*' -rl ./yaml/singlerouting`
sed -i "s/max.connections.size.per.query: [0-9]*}/max.connections.size.per.query: $5}/g" `grep 'max.connections.size.per.query: [0-9]*' -rl ./yaml/singlerouting`
sed -i "s/max.connections.size.per.query: [0-9]*}/max.connections.size.per.query: $5}/g" `grep 'max.connections.size.per.query: [0-9]*' -rl ./yaml/server.yaml`

sed -i "s/shardingsphere.sharding.table.count=[0-9]*/shardingsphere.sharding.table.count=$2/g" `grep 'shardingsphere.sharding.table.count=[0-9]*' -rl ./config/user-config.properties`
sed -i "s/shardingsphere.sharding.db.count=[0-9]*/shardingsphere.sharding.db.count=$1/g" `grep 'shardingsphere.sharding.db.count=[0-9]*' -rl ./config/user-config.properties`
sed -i "s/shardingsphere.minimum.connection.count=[0-9]*/shardingsphere.minimum.connection.count=$3/g" `grep 'shardingsphere.minimum.connection.count=[0-9]*' -rl ./config/user-config.properties`
sed -i "s/shardingsphere.maximum.connection.count=[0-9]*/shardingsphere.maximum.connection.count=$4/g" `grep 'shardingsphere.maximum.connection.count=[0-9]*' -rl ./config/user-config.properties`
sed -i "s/shardingsphere.maximum.connection.count.for.each.query=[0-9]*/shardingsphere.maximum.connection.count.for.each.query=$5/g" `grep 'shardingsphere.maximum.connection.count.for.each.query=[0-9]*' -rl ./config/user-config.properties`

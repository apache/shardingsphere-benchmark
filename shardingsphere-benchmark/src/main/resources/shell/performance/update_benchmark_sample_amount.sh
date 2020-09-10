benchmark_resource_dir="/export/benchmark/shardingsphere-benchmark/src/main/resources"
chmod -R 777 $benchmark_resource_dir
cd $benchmark_resource_dir
sed -i "s/shardingsphere.result.sample.amount=[0-9]*,[0-9]*/shardingsphere.result.sample.amount=$1,$2/g" `grep 'shardingsphere.result.sample.amount=[0-9]*,[0-9]*' -rl ./config/user-config.properties`
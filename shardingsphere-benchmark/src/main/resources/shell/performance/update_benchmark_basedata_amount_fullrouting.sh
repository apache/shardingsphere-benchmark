benchmark_resource_dir="/export/benchmark/shardingsphere-benchmark/src/main/resources"
chmod -R 777 $benchmark_resource_dir
cd $benchmark_resource_dir
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/common/ss/encrypt/ss-encrypt-createdata-testplan.jmx`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/common/ss/encrypt/ss-encrypt-createdata-testplan.jmx`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/common/ss/masterslave/ss-masterslave-createdata-testplan.jmx`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/common/ss/masterslave/ss-masterslave-createdata-testplan.jmx`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/common/ss/sharding/ss-sharding-createdata-testplan.jmx`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/common/ss/sharding/ss-sharding-createdata-testplan.jmx`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/common/ss/sharding-masterslave-encrypt/ss-sharding-masterslave-encrypt-createdata-testplan.jmx`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/common/ss/sharding-masterslave-encrypt/ss-sharding-masterslave-encrypt-createdata-testplan.jmx`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/common/jdbc/encrypt/jdbc-encrypt-createdata-testplan.jmx`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/common/jdbc/encrypt/jdbc-encrypt-createdata-testplan.jmx`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/common/jdbc/masterslave/jdbc-masterslave-createdata-testplan.jmx`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/common/jdbc/masterslave/jdbc-masterslave-createdata-testplan.jmx`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/common/jdbc/sharding/jdbc-sharding-createdata-testplan.jmx`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/common/jdbc/sharding/jdbc-sharding-createdata-testplan.jmx`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/common/jdbc/shardingmasterslaveencrypt/jdbc-shardingmasterslaveencrypt-createdata-testplan.jmx`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/common/jdbc/shardingmasterslaveencrypt/jdbc-shardingmasterslaveencrypt-createdata-testplan.jmx`
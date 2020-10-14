#!bin/bash
benchmark_resource_dir="/export/benchmark/shardingsphere-benchmark/src/main/resources"
chmod -R 777 $benchmark_resource_dir
cd $benchmark_resource_dir

sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/fullrouting`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/fullrouting`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/rangerouting`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/rangerouting`
sed -i "s/\"ThreadGroup.num_threads\">[0-9]*/\"ThreadGroup.num_threads\">$2/g" `grep '"ThreadGroup.num_threads">[0-9]*' -rl ./testplan/singlerouting`
sed -i "s/\"LoopController.loops\">[0-9]*/\"LoopController.loops\">$1/g" `grep '"LoopController.loops">[0-9]*' -rl ./testplan/singlerouting`
sed -i "s/shardingsphere.jmeter.concurrency.count=[0-9]*/shardingsphere.jmeter.concurrency.count=$2/g" `grep 'shardingsphere.jmeter.concurrency.count=[0-9]*' -rl ./config/user-config.properties`
sed -i "s/shardingsphere.jmeter.loop.count=[0-9]*/shardingsphere.jmeter.loop.count=$1/g" `grep 'shardingsphere.jmeter.loop.count=[0-9]*' -rl ./config/user-config.properties`
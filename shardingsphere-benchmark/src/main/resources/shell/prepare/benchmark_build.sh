export JAVA_HOME=/export/jdk1.8.0_192/
export CLASSPATH=.:$JAVA_HOME/jre/lib/rt.jar:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar
export PATH=$PATH:$JAVA_HOME/bin

proxy_work_dir="/home/jenkins"
benchmark_result_parent_dir="/export/shardingsphere-benchmark"
benchmark_base_dir="/export/benchmark/shardingsphere-benchmark"
maven_base_dir="/export/apache-maven-3.6.0"

cd $benchmark_base_dir

$maven_base_dir/bin/mvn clean install

scp -r target/shardingsphere-benchmark-1.1-SNAPSHOT.jar jenkins@ss.benchmark.performance.machine:$proxy_work_dir/apache-jmeter-4.0/lib/ext

scp -r src/main/resources/testplan/* jenkins@ss.benchmark.performance.machine:$proxy_work_dir/apache-jmeter-4.0/shardingsphere_test_plan

#copy yaml of proxy to proxy machine:/home/jenkins/BT_jenkins/proxy_yaml_conf
scp -r src/main/resources/yaml/* jenkins@ss.benchmark.proxy.machine:$benchmark_result_parent_dir/yaml_conf

scp -r src/main/resources/shell/deploy/* jenkins@ss.benchmark.proxy.machine:$benchmark_result_parent_dir/shell

#copy benchmark jar to proxy machine
scp -r target/shardingsphere-benchmark-1.1-SNAPSHOT.jar jenkins@ss.benchmark.proxy.machine:$proxy_work_dir

#scp -r ss_build_version.log jenkins@ss.benchmark.performance.machine:$proxy_work_dir/apache-jmeter-4.0
#scp -r ss_build_version.log jenkins@ss.benchmark.proxy.machine:$proxy_work_dir/BT_jenkins

scp -r src/main/resources/shell/prepare/jmeter_result_manage.sh jenkins@ss.benchmark.performance.machine:$benchmark_result_parent_dir/shell

scp -r src/main/resources/shell/performance/update_benchmark_basedata_amount_fullrouting.sh jenkins@ss.benchmark.performance.machine:$benchmark_result_parent_dir/shell
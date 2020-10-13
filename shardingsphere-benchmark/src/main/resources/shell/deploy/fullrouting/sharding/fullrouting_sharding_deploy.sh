#!/bin/sh
proxy_work_dir="/home/jenkins"
proxy_conf_dir="/export/shardingsphere-benchmark/yaml_conf"
if [ ! -d $proxy_work_dir  ];then
  mkdir -p $proxy_work_dir
fi

cd $proxy_work_dir
./shardingsphere-proxy-distribution-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/bin/stop.sh
sleep 5
rm -f  ./shardingsphere-proxy-distribution-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/conf/config-*.yaml
cp -f $proxy_conf_dir/fullrouting/sharding/proxy/config-proxy-fullrouting-sharding.yaml $proxy_conf_dir/server.yaml ./shardingsphere-proxy-distribution-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/conf
./shardingsphere-proxy-distribution-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/bin/start.sh
sleep 10

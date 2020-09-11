#!/bin/sh
proxy_work_dir="/home/jenkins"
proxy_conf_dir="/export/shardingsphere-benchmark/yaml_conf"
if [ ! -d $proxy_work_dir  ];then
  mkdir -p $proxy_work_dir
fi

cd $proxy_work_dir
./apache-shardingsphere-*-shardingsphere-proxy-bin/bin/stop.sh
sleep 5
rm -f  ./apache-shardingsphere-*-shardingsphere-proxy-bin/conf/config-*.yaml
cp -f $proxy_conf_dir/rangerouting/sharding/proxy/config-proxy-rangerouting-sharding.yaml $proxy_conf_dir/server.yaml ./apache-shardingsphere-*-shardingsphere-proxy-bin/conf
./apache-shardingsphere-*-shardingsphere-proxy-bin/bin/start.sh
sleep 10

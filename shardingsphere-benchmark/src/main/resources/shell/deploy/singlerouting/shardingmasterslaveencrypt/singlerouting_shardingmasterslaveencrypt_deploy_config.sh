#!/bin/sh
proxy_work_dir="/home/jenkins"
if [ ! -d $proxy_work_dir  ];then
  mkdir -p $proxy_work_dir
fi

cd $proxy_work_dir
rm -fr apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin
mv shardingsphere-distribution/shardingsphere-proxy-distribution/target/*shardingsphere-proxy-bin.tar.gz .
tar zxvf apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin.tar.gz
cp -f ./mysql-connector-java-5.1.47.jar ./shardingsphere-benchmark-1.1-SNAPSHOT.jar ./apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/lib
sleep 10
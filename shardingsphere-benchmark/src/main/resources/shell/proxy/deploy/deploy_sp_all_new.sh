#!/bin/sh
cd /home/jenkins
./apache-shardingsphere-*-shardingsphere-proxy-bin/bin/stop.sh
sleep 5
#rm -fr apache-shardingsphere-*-sharding-proxy-bin

#mv sharding-proxy/target/*.tar.gz .
#mv sharding-distribution/sharding-proxy-distribution/target/*sharding-proxy-bin.tar.gz .
#tar zxvf apache-shardingsphere-*-sharding-proxy-bin.tar.gz
##rm -fr sharding-proxy *.tar.gz
####copy conf
rm -f  /home/jenkins/apache-shardingsphere-*-shardingsphere-proxy-bin/conf/config-*.yaml
#mv /home/jenkins/apache-shardingsphere-*-sharding-proxy-bin/conf/config-sharding_alldb_alltable.yaml /home/jenkins/apache-shardingsphere-*-sharding-proxy-bin/conf/config-sharding_alldb_alltable.yaml.bak
cp -f /home/jenkins/BT_jenkins/sp_conf/config-sharding_alldb_singletable.yaml /home/jenkins/BT_jenkins/sp_conf/server.yaml /home/jenkins/apache-shardingsphere-*-shardingsphere-proxy-bin/conf
#cp -f /home/jenkins/mysql-connector-java-5.1.47.jar /home/jenkins/apache-shardingsphere-*-sharding-proxy-bin/lib
./apache-shardingsphere-*-shardingsphere-proxy-bin/bin/start.sh
sleep 10
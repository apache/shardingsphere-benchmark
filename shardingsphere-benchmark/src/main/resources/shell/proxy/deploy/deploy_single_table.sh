#!/bin/sh
cd /home/jenkins
./apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/bin/stop.sh
sleep 5
rm -fr apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin

#mv sharding-proxy/target/*.tar.gz .
mv shardingsphere-distribution/shardingsphere-proxy-distribution/target/*shardingsphere-proxy-bin.tar.gz .
tar zxvf apache-shardingsphere-5.0.config-master_slave_enc_sharding.yaml0-RC1-SNAPSHOT-shardingsphere-proxy-bin.tar.gz
##rm -fr sharding-proxy *.tar.gz
####copy conf
rm -f /home/jenkins/apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/conf/config-*
cp -f /home/jenkins/BT_jenkins/sp_conf/config-sharding_alldb_alltable.yaml /home/jenkins/BT_jenkins/sp_conf/server.yaml /home/jenkins/apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/conf
cp -f /home/jenkins/mysql-connector-java-5.1.47.jar /home/jenkins/apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/lib
./apache-shardingsphere-5.0.0-RC1-SNAPSHOT-shardingsphere-proxy-bin/bin/start.sh
sleep 5
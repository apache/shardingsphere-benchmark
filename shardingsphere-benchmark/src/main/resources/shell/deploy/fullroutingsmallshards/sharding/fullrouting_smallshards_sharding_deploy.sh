ss_version=`head -n +1 /home/jenkins/BT_jenkins/ss_build_version.log`

if [ $ss_version == 5.0 ];then
  echo "5.0"
  sh /export/shardingsphere-benchmark/shell/fullrouting/sharding/fullrouting_smallshards_sharding_deploy_5.0.sh
elif [ $ss_version == 4.1.1 ];then
  echo "4.1.1"
  sh /export/shardingsphere-benchmark/shell/fullrouting/sharding/fullrouting_smallshards_sharding_deploy_4.0.sh
else
  echo "default version"
  sh /export/shardingsphere-benchmark/shell/fullrouting/sharding/fullrouting_smallshards_sharding_deploy_5.0.sh
fi

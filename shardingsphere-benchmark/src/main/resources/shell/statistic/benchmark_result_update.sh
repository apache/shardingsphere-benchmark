benchmark_base_dir="/export/benchmark_project/shardingsphere-benchmark"
benchmark_result_base_dir="/export/shardingsphere-benchmark/result"
cd $benchmark_base_dir

if [ -f ./ss_build_version.log ]; then
  rm ./ss_build_version.log
fi
echo $1 > ./ss_build_version.log
sleep 2s

ss_version=`head -n +1 ./ss_build_version.log`

if [ $1 == 5.0 ];then
  echo "5.0"
  git reset --hard
  git checkout master
  git reset --hard
  git pull origin master
  cp $benchmark_result_base_dir/shardingsphere-benchmark.xls $benchmark_base_dir/shardingsphere-benchmark/resources/report
  cp $benchmark_result_base_dir/shardingsphere-benchmark-avg.xls $benchmark_base_dir/shardingsphere-benchmark/resources/report
  git add .
  git commit -m "update benchmark result"
  git push origin master
elif [ $1 == 4.1.1 ];then
  echo "4.1.1"
  git reset --hard
  git checkout 4.1.1
  git reset --hard
  git pull origin 4.1.1
  cp $benchmark_result_base_dir/shardingsphere-benchmark.xls $benchmark_base_dir/shardingsphere-benchmark/resources/report
  cp $benchmark_result_base_dir/shardingsphere-benchmark-avg.xls $benchmark_base_dir/shardingsphere-benchmark/resources/report
  git add .
  git commit -m "update benchmark result"
  git push origin 4.1.1
else
  echo "default version"
  git reset --hard
  git checkout master
  git reset --hard
  git pull origin master
  cp $benchmark_result_base_dir/shardingsphere-benchmark.xls $benchmark_base_dir/shardingsphere-benchmark/resources/report
  cp $benchmark_result_base_dir/shardingsphere-benchmark-avg.xls $benchmark_base_dir/shardingsphere-benchmark/resources/report
  git add .
  git commit -m "update benchmark result"
  git push origin master  
fi


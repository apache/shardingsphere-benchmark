benchmark_base_dir="/export/benchmark/shardingsphere-benchmark"
chmod -R 777 $benchmark_base_dir
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
elif [ $1 == 4.1.1 ];then
  echo "4.1.1"
  git reset --hard
  git checkout 4.1.1
  git reset --hard
  git pull origin 4.1.1
else
  echo "default version"
  git reset --hard
  git checkout master
  git reset --hard
  git pull origin master
fi

sed -i "s/shardingsphere.version=[^0-9.]*\([0-9.]*\)\([0-9.]*\)./shardingsphere.version=$1/g" `grep 'shardingsphere.version=[^0-9.]*\([0-9.]*\)\([0-9.]*\)' -rl ./src/main/resources/config/user-config.properties`


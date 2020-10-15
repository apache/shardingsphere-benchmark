benchmark_result_dir="/export/shardingsphere-benchmark/result"
# create base dir for benchmark result
if [ ! -d $benchmark_result_dir  ];then
  mkdir -p $benchmark_result_dir
fi
chmod -R 777 $benchmark_result_dir

cd $benchmark_result_dir

# create sub dir of benchmark result
if [ ! -d "./fullrouting/encrypt" ];then
  mkdir -p ./fullrouting/encrypt
else
  echo ./fullrouting/encrypt/  exist
  rm -rf ./fullrouting/encrypt/*
fi

if [ ! -d "./fullrouting/masterslave" ];then
  mkdir -p ./fullrouting/masterslave
else
  echo ./fullrouting/masterslave  exist
  rm -rf ./fullrouting/masterslave/*
fi

if [ ! -d "./fullrouting/sharding" ];then
  mkdir -p ./fullrouting/sharding
else
  echo ./fullrouting/sharding  exist
  rm -rf ./fullrouting/sharding/*
fi

if [ ! -d "./fullrouting/shardingmasterslaveencrypt" ];then
  mkdir -p ./fullrouting/shardingmasterslaveencrypt
else
  echo ./fullrouting/shardingmasterslaveencrypt  exist
  rm -rf ./fullrouting/shardingmasterslaveencrypt/*
fi

if [ ! -d "./rangerouting/encrypt" ];then
  mkdir -p ./rangerouting/encrypt
else
  echo ./rangerouting/encrypt  exist
  rm -rf ./rangerouting/encrypt/*
fi

if [ ! -d "./rangerouting/masterslave" ];then
  mkdir -p ./rangerouting/masterslave
else
  echo ./rangerouting/masterslave  exist
  rm -rf ./rangerouting/masterslave/*
fi

if [ ! -d "./rangerouting/sharding" ];then
  mkdir -p ./rangerouting/sharding
else
  echo ./rangerouting/sharding  exist
  rm -rf ./rangerouting/sharding/*
fi

if [ ! -d "./rangerouting/shardingmasterslaveencrypt" ];then
  mkdir -p ./rangerouting/shardingmasterslaveencrypt
else
  echo ./rangerouting/shardingmasterslaveencrypt  exist
  rm -rf ./rangerouting/shardingmasterslaveencrypt/*
fi

if [ ! -d "./singlerouting/encrypt" ];then
  mkdir -p ./singlerouting/encrypt
else
  echo ./singlerouting/encrypt  exist
  rm -rf ./singlerouting/encrypt/*
fi

if [ ! -d "./singlerouting/masterslave" ];then
  mkdir -p ./singlerouting/masterslave
else
  echo ./singlerouting/masterslave  exist
  rm -rf ./singlerouting/masterslave/*
fi

if [ ! -d "./singlerouting/sharding" ];then
  mkdir -p ./singlerouting/sharding
else
  echo ./singlerouting/sharding  exist
  rm -rf ./singlerouting/sharding/*
fi

if [ ! -d "./singlerouting/shardingmasterslaveencrypt" ];then
  mkdir -p ./singlerouting/shardingmasterslaveencrypt
else
  echo ./singlerouting/shardingmasterslaveencrypt  exist
  rm -rf ./singlerouting/shardingmasterslaveencrypt/*
fi

chmod -R 777 ./
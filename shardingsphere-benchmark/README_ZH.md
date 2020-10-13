# ShardingSphere Benchmark是什么
ShardingSphere Benchmark是测试ShardingSphere的性能基线工具，目前覆盖的核心场景如下表。 覆盖的测试总量为96，计算公式 3(Scenarios) * 4(Products) * 2(SQL Type) * 4(Rules)。

* Full Route：所执行sql会路由到所有物理表中。 
* Range Route：所执行sql会路由到部分物理表中。
* Single Route：所执行sql只会路由到一张物理表中。

|       *Scenarios*       |  *Products*           |      *SQL TYPE*        | *Rules*  | 
| ----------------------- | --------------------- | ---------------------- | ------------------------ |
| Full Route              |  ShardingSphere-JDBC  |  Select                |   Encrypt                | 
| Range Route             |  ShardingSphere-Proxy |  Insert/Update/Delete  |   Master-Slave           |  
| Single Route            |  MYSQL                |                        |   Sharding               | 
|                         |                       |                        |   Sharding-Master-Slave-Encrypt   | 



## ShardingSphere Benchmark 项目结构
更全面的理解ShardingSphere Benchmark项目结构，会帮助你轻松运行ShardingSphere Benchmark相关测试。请参考下面目录及注释。

```
project
│   README.md
│   README_ZH.md    
│
└───src/main/java
│   │     
│   └───org/apache/shardingsphere/benchmark
│       │   
│       └───org/apache/shardingsphere/benchmark/bean
│       │
│       └───org/apache/shardingsphere/benchmark/common
│       │       └───org/apache/shardingsphere/benchmark/common/file           // 文件修改相关操作模块，处理文件类型包括 yaml，xml，properties。
│       │       └───org/apache/shardingsphere/benchmark/common/statistic      // JMeter生成结果统计模块。
│       │       
│       └───org/apache/shardingsphere/benchmark/db                             // 数据库操作模块，包括增删改查。
│       │ 
│       └───org/apache/shardingsphere/benchmark/jemeter                        // 不同场景下的JMeter测试计划。
│       │       └───org/apache/shardingsphere/benchmark/jemeter/common         // 初始化基础数据及ShardingSphere benchmark测试需要的物理库/物理表。
│       │       └───org/apache/shardingsphere/benchmark/jemeter/fullrouting    // 对于ShardingSphere-Proxy 和 ShardingSphere-JDBC产品在全路由场景下的测试计划，包括基于不同的规则encrypt，master-slave，sharding，sharding-master-slave-encrypt。
│       │       └───org/apache/shardingsphere/benchmark/jemeter/rangerouting   // 对于ShardingSphere-Proxy 和 ShardingSphere-JDBC产品在范围路由场景下的测试计划，包括基于不同的规则encrypt，master-slave，sharding，sharding-master-slave-encrypt。
│       │       └───org/apache/shardingsphere/benchmark/jemeter/singlerouting  // 对于ShardingSphere-Proxy 和 ShardingSphere-JDBC产品在单路由场景下的测试计划，包括基于不同的规则encrypt，master-slave，sharding，sharding-master-slave-encrypt。
└───src/main/resources
│   │     
│   └───common
│   │    │
│   │    └───config
│   │    │ initconfig-testplan.jmx                      // 初始化ShardingSphere Benchmark测试需要的物理库和物理表。
│   │    └───datapreparation
│   │   │   │
│   │   │   └───jdbc                                    // MYSQL产品的基础数据初始化。
│   │   │   │ jdbc-cleardata-testplan.jmx               // MYSQL基础数据清除。
│   │   │   │ jdbc-createdata-testplan.jmx              // MYSQL基础数据创建。
│   │   │   └───shardingsphere                          // ShardingSphere-JDBC 和 ShardingSphere-Proxy基础数据初始化，包括基于不同的规则encrypt，master-slave，sharding，sharding-master-slave-encrypt。
│   │   │   │   │ 
│   │   │   │   └───encrypt                             
│   │   │   │   │ shardingsphere-encrypt-cleardata-testplan.jmx            // 基于脱敏规则ShardingSphere-JDBC和ShardingSphere-Proxy产品的基础数据清除。 
│   │   │   │   │ shardingsphere-encrypt-createdata-testplan.jmx           // 基于脱敏规则ShardingSphere-JDBC和ShardingSphere-Proxy产品的基础数据创建。 
│   │   │   │   └───masterslave
│   │   │   │   │ shardingsphere-masterslave-cleardata-testplan.jmx        // 基于主从规则ShardingSphere-JDBC和ShardingSphere-Proxy产品的基础数据清除。 
│   │   │   │   │ shardingsphere-masterslave-createdata-testplan.jmx       // 基于主从规则ShardingSphere-JDBC和ShardingSphere-Proxy产品的基础数据创建。 
│   │   │   │   └───sharding
│   │   │   │   │ shardingsphere-sharding-cleardata-testplan.jmx           // 基于分片规则ShardingSphere-JDBC和ShardingSphere-Proxy产品的基础数据清除。 
│   │   │   │   │ shardingsphere-sharding-createdata-testplan.jmx          // 基于分片规则ShardingSphere-JDBC和ShardingSphere-Proxy产品的基础数据创建。 
│   │   │   │   └───sharding-masterslave-encrypt
│   │   │   │   │ shardingsphere-sharding-masterslave-encrypt-cleardata-testplan.jmx    // 基于混合规则分片-主从-脱敏ShardingSphere-JDBC和ShardingSphere-Proxy产品的基础数据清除。
│   │   │   │   │ shardingsphere-sharding-masterslave-encrypt-createdata-testplan.jmx   // 基于混合规则分片-主从-脱敏ShardingSphere-JDBC和ShardingSphere-Proxy产品的基础数据创建。
│   │   │   │   │
│   └───testplan                                           
│   │    │       
│   │    └───fullrouting                // 全路由场
│   │   │   │   │
│   │   │   │   └───encrypt            //脱敏规则
│   │   │   │   │ jdbc-fullrouting-encrypt-insertupdatedelete-testplan.jmx               // 基于脱敏规则，MYSQL产品全路由场景下的insert-update-delete测试计划。  
│   │   │   │   │ jdbc-fullrouting-encrypt-select-testplan.jmx                           // 基于脱敏规则，MYSQL产品全路由场景下的select测试计划。 
│   │   │   │   │ proxy-fullrouting-encrypt-insertupdatedelete-testplan.jmx              // 基于脱敏规则，ShardingSphere-Proxy产品全路由场景下的insert-update-delete测试计划。  
│   │   │   │   │ proxy-fullrouting-encrypt-select-testplan.jmx                          // 基于脱敏规则，ShardingSphere-Proxy产品全路由场景下的select测试计划。 
│   │   │   │   │ shardingjdbc-fullrouting-encrypt-insertupdatedelete-testplan.jmx       // 基于脱敏规则，ShardingSphere-JDBC产品全路由场景下的insert-update-delete测试计划。
│   │   │   │   │ shardingjdbc-fullrouting-encrypt-select-testplan.jmx                   // 基于脱敏规则，ShardingSphere-JDBC产品全路由场景下的select测试计划。
│   │   │   │   └───masterslave        //主从规则
│   │   │   │   │ proxy-fullrouting-masterslave-insertupdatedelete-testplan.jmx          // 基于主从规则，ShardingSphere-Proxy产品全路由场景下的insert-update-delete测试计划。  
│   │   │   │   │ proxy-fullrouting-masterslave-select-testplan.jmx                      // 基于主从规则，ShardingSphere-Proxy产品全路由场景下的select测试计划。 
│   │   │   │   │ shardingjdbc-fullrouting-masterslave-insertupdatedelete-testplan.jmx   // 基于主从规则，ShardingSphere-JDBC产品全路由场景下的insert-update-delete测试计划。  
│   │   │   │   │ shardingjdbc-fullrouting-masterslave-select-testplan.jmx               // 基于主从规则，ShardingSphere-JDBC产品全路由场景下的select测试计划。    
│   │   │   │   └───sharding           //sharding rule
│   │   │   │   │ proxy-fullrouting-sharding-insertupdatedelete-testplan.jmx             // 基于分片规则，ShardingSphere-Proxy产品全路由场景下的insert-update-delete测试计划。
│   │   │   │   │ proxy-fullrouting-sharding-select-testplan.jmx                         // 基于分片规则，ShardingSphere-Proxy产品全路由场景下的select测试计划。  
│   │   │   │   │ shardingjdbc-fullrouting-sharding-insertupdatedelete-testplan.jmx      // 基于分片规则，ShardingSphere-JDBC产品全路由场景下的insert-update-delete测试计划。
│   │   │   │   │ shardingjdbc-fullrouting-sharding-select-testplan.jmx                  // 基于分片规则，ShardingSphere-JDBC产品全路由场景下的select测试计划。 
│   │   │   │   └───sharding-masterslave-encrypt    //sharding-masterslave-encrypt rule       
│   │   │   │   │ proxy-fullrouting-shardingmasterslaveencrypt-insertupdatedelete-testplan.jmx          // 基于混合规则分片-主从-脱敏，ShardingSphere-Proxy产品全路由场景下的insert-update-delete测试计划。
│   │   │   │   │ proxy-fullrouting-shardingmasterslaveencrypt-select-testplan.jmx                      // 基于混合规则分片-主从-脱敏，ShardingSphere-Proxy产品全路由场景下的select测试计划。
│   │   │   │   │ shardingjdbc-fullrouting-shardingmasterslaveencrypt-insertupdatedelete-testplan.jmx   // 基于混合规则分片-主从-脱敏，ShardingSphere-JDBC产品全路由场景下的insert-update-delete测试计划。  
│   │   │   │   │ shardingjdbc-fullrouting-shardingmasterslaveencrypt-select-testplan.jmx               // 基于混合规则分片-主从-脱敏，ShardingSphere-JDBC产品全路由场景下的select测试计划。   
│   │   │   │   │
│   │   └───rangerouting  //所有范围路由相关的测试计划文件名称和全路由类似，可参考全路由场景，不在此一一列出所有文件。
│   │   │       
│   │   └───singlerouting //所有单路由相关的测试计划文件名称和全路由类似，可参考全路由场景，不在此一一列出所有文件。
│   │   │       
│   │   └───statistic     // 统计结果
│   │   │ ss-benchmark-statistic-testplan.jmx  //统计JMeter结果的测试计划，统计结果入库。
│   └───yaml
│   │    │   server.yaml                                           // ShardingSphere-Proxy 使用到的配置文件.
│   │    │   └───fullrouting                //全路由场景
│   │    │   │   └───encrypt                // 脱敏规则下的yaml配置文件
│   │    │   │   │   └───proxy
│   │    │   │   │   │ config-proxy-fullrouting-encrypt.yaml            // ShardingSphere-Proxy产品，配置脱敏规则的配置文件。
│   │    │   │   │   └───shardingjdbc                                   
│   │    │   │   │   │ config-shardingjdbc-fullrouting-encrypt.yaml     // ShardingSphere-JDBC产品，配置脱敏规则的配置文件。
│   │    │   │   └───masterslave
│   │    │   │   │   └───proxy
│   │    │   │   │   │ config-proxy-fullrouting-masterslave.yaml        // ShardingSphere-Proxy产品，配置主从规则的配置文件。
│   │    │   │   │   └───shardingjdbc
│   │    │   │   │   │ config-shardingjdbc-fullrouting-masterslave.yaml // ShardingSphere-JDBC产品，配置主从规则的配置文件。
│   │    │   │   └───sharding
│   │    │   │   │   └───proxy
│   │    │   │   │   │ config-proxy-fullrouting-sharding.yaml           // ShardingSphere-Proxy产品，配置分片规则的配置文件。
│   │    │   │   │   └───shardingjdbc
│   │    │   │   │   │ config-shardingjdbc-fullrouting-sharding.yaml    // ShardingSphere-JDBC产品，配置分片规则的配置文件。
│   │    │   │   └───sharding-masterslave-encrypt
│   │    │   │   │   └───proxy
│   │    │   │   │   │ config-proxy-fullrouting-sharding-masterslave-enc.yaml             // ShardingSphere-Proxy产品，配置混合规则 分片-主从-脱敏 规则的配置文件。
│   │    │   │   │   └───shardingjdbc
│   │    │   │   │   │ config-shardingjdbc-fullrouting-sharding-masterslave-encrypt.yaml  // ShardingSphere-JDBC产品，配置混合规则 分片-主从-脱敏 规则的配置文件。
│   │    │   └───rangerouting             // 范围路由下的所有yaml配置文件，文件名称和全路由类似，可参考全路由场景，一一列出所有文件。 
│   │    │   │
│   │    │   └───singlerouting            // 单路由下的所有yaml配置文件，文件名称和全路由类似，可参考全路由场景，一一列出所有文件。
```




## 运行 ShardingSphere Benchmark
简单来说ShardingSphere Benchmark主要基于JMeter实现的性能测试工具，执行命令 *jmeter -n -t testplan* 便可运行。

### 环境准备
由于用户环境的多样性，我们没有提供统一的自动化安装基础环境方式，所以请手动安装基础环境，如已安装，可忽略下列步骤。

#### 安装 MYSQL 
* 从官网下载MYSQL安装程序，最好为5.x以上版本，MYSQL官网地址：[MYSQL installation](https://dev.mysql.com/downloads/mysql/)。
* 参考MYSQL官方文档进行安装，请保持版本一致，MYSQL官方文档地址：[MYSQL Doc](https://dev.mysql.com/doc)。

#### 安装 JDK
* 从官网下载JDK安装程序，版本最好为JDK7以上，JDK下载地址：[JDK installation](https://www.oracle.com/java/technologies/javase-downloads.html)。
* 参考JDK官方文档进行安装配置，JDK官方文档地址： [JDK installation](https://www.oracle.com/java/technologies/javase-downloads.html)。

#### 安装 Maven
* 从官网下载Maven安装程序(http://maven.apache.org/download.cgi) from official website depending on the system you use。
* 参考Maven官方文档进行安装配置 [Maven doc](http://maven.apache.org/install.html)。

#### 获取 JDBC Jar
* 从官网下载正确版本的JDBC Jar文件。

#### 构建 ShardingSphere Benchmark
* 获取ShardingSphere Benchmark源码 [ShardingSphere Benchmark Code URL](https://github.com/wcsoft/shardingsphere-benchmark)。
* 按照如下命令构建ShardingSphere Benchmark。
```bash
mvn clean install
```

#### 安装 ShardingSphere-Proxy
* 获取ShardingSphere源码 [ShardingSphere Source Code URL](https://github.com/apache/shardingsphere), the features for different branches refer to [ShardingSphere Doc](https://shardingsphere.apache.org/)。
* 按照如下命令构建ShardingSphere，并获取ShardingSphere-Proxy安装包。对于ShardingSphere 4.x版本，ShardingSphere-Proxy安装包所在路径为 *{project_base_dir}/sharding-distribution/sharding-proxy-distribution/target*; 对于ShardingSphere 5.x版本，它的安装程序所在路径为 *{project_base_dir}/shardingsphere-distribution/shardingsphere-proxy-distribution/target*。
```bash
mvn clean install -Prelease
```
* 解压ShardingSphere-proxy，将以上获取的JDBC Jar和ShardingSphere Benchmark Jar 放在 *{ShardingSphere-Proxy_base_dir}/lib/* 路径下。
* 复制ShardingSphere Benchmark jar 和 JDBC jar到 *{ShardingSphere-Proxy_base_dir}/lib*路径下。
* 启动ShardingSphere-Proxy，启动脚步路径为 *{ShardingSphere-Proxy_base_dir}/bin*，根据当前使用的系统选择正确的启动脚步。


#### 安装 JMeter
* 从官网获取JMeter安装程序 [JMeter installation](https://jmeter.apache.org/download_jmeter.cgi)，版本最好选择5.3。
* 依照官方文档安装配置JMeter [JMeter doc](https://jmeter.apache.org/usermanual/get-started.html)。
* 拷贝ShardingSphere Benchmark jar到 *{JMeter_base_dir}/lib/ext* 路径下。


#### 配置
我们提取了ShardingSphere benchmark项目中所有用户相关配置，这些配置存在于 *{ShardingSphere Benchmark base dir}/src/main/resources/config/user-config.properties*文件中，所以需要根据当前运行环境进行修改。此外ShardingSphere benchmark工程中涉及到ip使用均以host方式进行配置，以防泄露引发安全问题，对于host配置已提供示例文件，文件路径为 *{ShardingSphere Benchmark base dir}/src/main/resources/config/baitiao_test_machline_hosts*，需要依据当前环境修改对应ip并添加到host文件中。如果仅在本地测试，无需替换ip。

```bash
// ShardingSphere工程根目录.
shardingsphere.project.base.path=/export/jenkins/workspace/ShardingSphere-Benchmark-Deploy
// ShardingSphere benchmark工程根目录。
shardingsphere.benchmark.project.base.path=/export/benchmark/shardingsphere-benchmark
// ShardingSphere benchmark结果输出根目录。
shardingsphere.benchmark.result.base.path=/export/shardingsphere-benchmark/result
// 存储ShardingSphere benchmark结果数据库端口号。
shardingsphere.benchmark.result.database.port=3306
// 存储ShardingSphere benchmark结果数据库用户名。
shardingsphere.benchmark.result.database.username=root
// 存储ShardingSphere benchmark结果数据库密码。
shardingsphere.benchmark.result.database.password=
// 存储ShardingSphere benchmark数据库机器ip列表或者host列表。
shardingsphere.benchmark.database.machine.host.list=ss.benchmark.fullrouting.encrypt.ds0;ss.benchmark.fullrouting.encrypt.ds1;ss.benchmark.fullrouting.encrypt.ds2
// 存储ShardingSphere benchmark结果数据库机器ip或者host。
shardingsphere.benchmark.result.database.host=ss.benchmark.result
```

#### 初始 物理库/物理表
运行ShardingSphere Benchmark前，需要创建对应的物理数据库和物理表；根据用户配置 分库数量/分表数量/ShardingSphereBenchmark结果输出根路径/ShardingSphere版本/并发量，进行统一自动化部署。以上配置均在 *{ShardingSphere Benchmark base dir}/src/main/resources/config/user-config.properties* 文件中，运行如下命令完成上述任务。
```bash
jmeter -n -t {ShardingSphere Benchmark base dir}/src/main/resources/testplan/common/config/initconfig-testplan.jmx
```

#### 初始 基础数据
所有ShardingSphere Benchmark测试场景会在固定基础数据条件下进行，创建测试数据计划文件存储在 *{ShardingSphere Benchmark base dir}/src/main/resources/testplan/common/datapreparation/*，执行如下命令完成基础数据创建。
```bash
jmeter -n -t {ShardingSphere Benchmark base dir}/src/main/resources/testplan/common/datapreparation/shardingsphere/sharding/shardingsphere-sharding-createdata-testplan.jmx
```

#### 运行 测试计划
所有的测试计划文件存在 *{ShardingSphere Benchmark base dir}/src/main/resources/testplan/*，提供如下示例命令，该命令完成ShardingJdbc产品在单路由，脱敏规则条件下执行select的基线采集。请根据需要选择你关注的场景。
```bash
jmeter -n -t {ShardingSphere Benchmark base dir}/src/main/resources/testplan/singlerouting/encrypt/shardingjdbc-singlerouting-encrypt-select-testplan.jmx
```

测试计划成功运行后，生成JMeter结果文件。JMeter生成结果片段如下图。
```bash
timeStamp,elapsed,label,responseCode,responseMessage,threadName,dataType,success,failureMessage,bytes,sentBytes,grpThreads,allThreads,Latency,IdleTime,Connect
1598274390236,8657,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-74,,true,,0,0,100,100,0,0,0
1598274390236,8657,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-65,,true,,0,0,100,100,0,0,0
1598274390236,8878,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-42,,true,,0,0,100,100,0,0,0
1598274390234,8917,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-15,,true,,0,0,100,100,0,0,0
1598274390234,9002,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-3,,true,,0,0,100,100,0,0,0
```

#### 统计
对不同场景下生成的JMeter结果进行统一汇总，统计结果主要以TPS为主。这些结果存储在两个物理表中，分别为 *benchmark_result* and *benchmark_avg_result*，同时将统计结果生成测试报告，报告文件格式为excel。

* 物理表 *benchmark_result*: 同一个场景下，每次测试生成的统计结果存储在该表中，表结构如下。

|       *字段*       |  *描述*           |   
| ----------------------- | --------------------- |
| id              |  主键，自动生成。  | 
| product         |  被测试产品包括ShardingJDBC，ShardingProxy 和 MYSQL。| 
| version         |  ShardingSphere 版本，配置在user-config.properties文件中。                | 
| scenario        |  三种测试场景包括FullRouting，RangeRouting，SingleRouting。 | 
| rules           |  四种规则包括 Encrypt， MasterSlave， Sharding， Sharding+Master+Slave+Encrypt。 | 
| tps             |  通过JMeter结果文件计算tps，计算公式为 *total count / total time*。 | 
| total           |  去除头尾的采样数据量。               | 
| maxCost         |  执行sql中，其中最长耗时。                | 
| minCost         |  执行sql中，其中最小耗时。  | 
| dbsql           |  实际运行的sql语句。                | 
| dboperation     |  执行的sql类型包括Select 和 Insert+Update+Delete。 | 
| concurrency     |  并发数据，配置在user-config.properties文件中。                | 
| tableshardingcount|  分表数量，配置在user-config.properties，yaml文件会使用到该值。                 | 
| dbshardingcount  |  分库数量，配置在user-config.properties，yaml文件会使用到该值。               | 

* 物理表 *benchmark_avg_result*: 同一各场景下，当前所有曾经生成的统计结果的平均值存储在该表中。

|       *字段*       |  *描述*           |   
| ----------------------- | --------------------- |
| id              |  主键，自动生成。  | 
| product         |  被测试产品包括ShardingJDBC，ShardingProxy 和 MYSQL。| 
| version         |  ShardingSphere 版本，配置在user-config.properties文件中。                | 
| scenario        |  三种测试场景包括FullRouting，RangeRouting，SingleRouting。 | 
| rules           |  四种规则包括 Encrypt， MasterSlave， Sharding， Sharding+Master+Slave+Encrypt。 | 
| avg_tps         |  同一测试场景下总tps平均值，计算公式 *total tps/total count*。 | 
| total           |  去除头尾的采样数据量。               | 
| maxCost         |  执行sql中，其中最长耗时。                | 
| minCost         |  执行sql中，其中最小耗时。  | 
| dbsql           |  实际运行的sql语句。                | 
| dboperation     |  执行的sql类型包括Select 和 Insert+Update+Delete。 | 
| concurrency     |  并发数据，配置在user-config.properties文件中。                | 
| tableshardingcount|  分表数量，配置在user-config.properties，yaml文件会使用到该值。                 | 
| dbshardingcount  |  分库数量，配置在user-config.properties，yaml文件会使用到该值。               | 

运行如下命令获取统计结果。
```bash
jmeter -n -t {ShardingSphere Benchmark base dir}/src/main/resources/testplan/statistic/ss-benchmark-statistic-testplan.jmx
```


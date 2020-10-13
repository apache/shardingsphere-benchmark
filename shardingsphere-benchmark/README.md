# What is ShardingSphere Benchmark
ShardingSphere Benchmark is a performance tool to check its benchmark of core scenarios which are in figure below. The amount of benchmark cases is 96, the formula is 3(Scenarios) * 4(Products) * 2(SQL Type) * 4(Rules).

* Full Route: sql will route to all of physical tables. 
* Range Route: sql will route to parts of physical tables.
* Single Route: sql will route to only one physical table.

|       *Scenarios*       |  *Products*           |      *SQL TYPE*        | *Rules*  | 
| ----------------------- | --------------------- | ---------------------- | ------------------------ |
| Full Route              |  ShardingSphere-JDBC  |  Select                |   Encrypt                | 
| Range Route             |  ShardingSphere-Proxy |  Insert/Update/Delete  |   Master-Slave           |  
| Single Route            |  MYSQL                |                        |   Sharding               | 
|                         |                       |                        |   Sharding-Master-Slave-Encrypt   | 

## ShardingSphere Benchmark Project Structure
It's much help to comprehend ShardingSphere Benchmark project structure to perform benchmark test easily. Refer to list below.

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
│       │       └───org/apache/shardingsphere/benchmark/common/file           // File common action such as modification of property file, xml or yaml.
│       │       └───org/apache/shardingsphere/benchmark/common/statistic      // Statistics for JMeter result.
│       │       
│       └───org/apache/shardingsphere/benchmark/db                             // Database action inlcuding querying, inserting, updating, deleting.
│       │ 
│       └───org/apache/shardingsphere/benchmark/jemeter                        // JMeter test plan for different scenarios.
│       │       └───org/apache/shardingsphere/benchmark/jemeter/common         // Initialization for base data and physical databases/tables. 
│       │       └───org/apache/shardingsphere/benchmark/jemeter/fullrouting    // JMeter test plan for full routing scenario with different rules such as encrypt, master-slave, sharding, sharding-master-slave-encrypt for ShardingSphere-Proxy and ShardingSphere-JDBC.
│       │       └───org/apache/shardingsphere/benchmark/jemeter/rangerouting   // JMeter test plan for range routing scenario with different rules such as encrypt, master-slave, sharding, sharding-master-slave-encrypt for ShardingSphere-Proxy and ShardingSphere-JDBC.
│       │       └───org/apache/shardingsphere/benchmark/jemeter/singlerouting  // JMeter test plan for single routing scenario with different rules such as encrypt, master-slave, sharding, sharding-master-slave-encrypt for ShardingSphere-Proxy and ShardingSphere-JDBC.
└───src/main/resources
│   │     
│   └───common
│   │    │
│   │    └───config
│   │    │ initconfig-testplan.jmx                      // Initialization for creating databases/tables for benchmark and bencmark result.
│   │    └───datapreparation
│   │   │   │
│   │   │   └───jdbc                                    //Base data initialization for different scenarios to MYSQL.
│   │   │   │ jdbc-cleardata-testplan.jmx               // Clear base data for MYSQL.
│   │   │   │ jdbc-createdata-testplan.jmx              // Create base data for MYSQL.
│   │   │   └───shardingsphere                          // Base data initialization for different scenarios to ShardingSphere-JDBC and ShardingSphere-Proxy.
│   │   │   │   │ 
│   │   │   │   └───encrypt                             
│   │   │   │   │ shardingsphere-encrypt-cleardata-testplan.jmx            // Clear base data with encrypt rule for ShardingSphere-JDBC and ShardingSphere-Proxy. 
│   │   │   │   │ shardingsphere-encrypt-createdata-testplan.jmx           // Create base data with encrypt rule for ShardingSphere-JDBC and ShardingSphere-Proxy. 
│   │   │   │   └───masterslave
│   │   │   │   │ shardingsphere-masterslave-cleardata-testplan.jmx        // Clear base data with masterslave rule for ShardingSphere-JDBC and ShardingSphere-Proxy. 
│   │   │   │   │ shardingsphere-masterslave-createdata-testplan.jmx       // Create base data with masterslave rule for ShardingSphere-JDBC and ShardingSphere-Proxy. 
│   │   │   │   └───sharding
│   │   │   │   │ shardingsphere-sharding-cleardata-testplan.jmx           // Clear base data with sharding rule for ShardingSphere-JDBC and ShardingSphere-Proxy. 
│   │   │   │   │ shardingsphere-sharding-createdata-testplan.jmx          // Create base data with sharding rule for ShardingSphere-JDBC and ShardingSphere-Proxy. 
│   │   │   │   └───sharding-masterslave-encrypt
│   │   │   │   │ shardingsphere-sharding-masterslave-encrypt-cleardata-testplan.jmx    // Clear base data with sharding-masterslave-encrypt rule for ShardingSphere-JDBC and ShardingSphere-Proxy. 
│   │   │   │   │ shardingsphere-sharding-masterslave-encrypt-createdata-testplan.jmx   // Create base data with sharding-masterslave-encrypt rule for ShardingSphere-JDBC and ShardingSphere-Proxy. 
│   │   │   │   │
│   └───testplan                                           
│   │    │       
│   │    └───fullrouting                // Full routine scenario
│   │   │   │   │
│   │   │   │   └───encrypt            //Encrypt rule
│   │   │   │   │ jdbc-fullrouting-encrypt-insertupdatedelete-testplan.jmx               // For MYSQL, insert-update-delete test plan with encrypt rule.  
│   │   │   │   │ jdbc-fullrouting-encrypt-select-testplan.jmx                           // For MYSQL, select test plan with encrypt rule.
│   │   │   │   │ proxy-fullrouting-encrypt-insertupdatedelete-testplan.jmx              // For ShardingSphere-Proxy, insert-update-delete test plan with encrypt rule.
│   │   │   │   │ proxy-fullrouting-encrypt-select-testplan.jmx                          // For ShardingSphere-Proxy, select test plan with encrypt rule.
│   │   │   │   │ shardingjdbc-fullrouting-encrypt-insertupdatedelete-testplan.jmx       // For ShardingSphere-JDBC, insert-update-delete test plan with encrypt rule.
│   │   │   │   │ shardingjdbc-fullrouting-encrypt-select-testplan.jmx                   // For ShardingSphere-JDBC, select test plan with encrypt rule.
│   │   │   │   └───masterslave        //master-slave rule
│   │   │   │   │ proxy-fullrouting-masterslave-insertupdatedelete-testplan.jmx          // For ShardingSphere-Proxy, insert-update-delete test plan with master-slave rule.  
│   │   │   │   │ proxy-fullrouting-masterslave-select-testplan.jmx                      // For ShardingSphere-Proxy, select test plan with master-slave rule.  
│   │   │   │   │ shardingjdbc-fullrouting-masterslave-insertupdatedelete-testplan.jmx   // For ShardingSphere-JDBC, insert-update-delete test plan with master-slave rule.  
│   │   │   │   │ shardingjdbc-fullrouting-masterslave-select-testplan.jmx               // For ShardingSphere-JDBC, select test plan with master-slave rule.  
│   │   │   │   └───sharding           //sharding rule
│   │   │   │   │ proxy-fullrouting-sharding-insertupdatedelete-testplan.jmx             // For ShardingSphere-Proxy, insert-update-delete test plan with sharding rule.  
│   │   │   │   │ proxy-fullrouting-sharding-select-testplan.jmx                         // For ShardingSphere-Proxy, select test plan with sharding rule.  
│   │   │   │   │ shardingjdbc-fullrouting-sharding-insertupdatedelete-testplan.jmx      // For ShardingSphere-JDBC, insert-update-delete test plan with sharding rule.  
│   │   │   │   │ shardingjdbc-fullrouting-sharding-select-testplan.jmx                  // For ShardingSphere-JDBC, select test plan with sharding rule.  
│   │   │   │   └───sharding-masterslave-encrypt    //sharding-masterslave-encrypt rule       
│   │   │   │   │ proxy-fullrouting-shardingmasterslaveencrypt-insertupdatedelete-testplan.jmx          // For ShardingSphere-Proxy, insert-update-delete test plan with sharding-master-slave-encrypt rule.  
│   │   │   │   │ proxy-fullrouting-shardingmasterslaveencrypt-select-testplan.jmx                      // For ShardingSphere-Proxy, select test plan with sharding-master-slave-encryp rule.  
│   │   │   │   │ shardingjdbc-fullrouting-shardingmasterslaveencrypt-insertupdatedelete-testplan.jmx   // For ShardingSphere-JDBC, insert-update-delete test plan with sharding-master-slave-encryp rule.  
│   │   │   │   │ shardingjdbc-fullrouting-shardingmasterslaveencrypt-select-testplan.jmx               // For ShardingSphere-JDBC, select test plan with sharding-master-slave-encryp rule.  
│   │   │   │   │
│   │   └───rangerouting  // Range routine scenario, file names of its test plans are similar with ones under *fullrouting* directory, please refer to them. 
│   │   │       
│   │   └───singlerouting // Single routine scenario, file names of its test plans are similar with ones under *fullrouting* directory, please refer to them. 
│   │   │       
│   │   └───statistic    
│   │   │ ss-benchmark-statistic-testplan.jmx  
│   └───yaml
│   │    │   server.yaml                                           // It's used by ShardingSphere-Proxy.
│   │    │   └───fullrouting                //Full routine
│   │    │   │   └───encrypt                // Encrypt rule.
│   │    │   │   │   └───proxy
│   │    │   │   │   │ config-proxy-fullrouting-encrypt.yaml            // For ShardingSphere-Proxy, it's for full route with encrypt rule.
│   │    │   │   │   └───shardingjdbc                                   
│   │    │   │   │   │ config-shardingjdbc-fullrouting-encrypt.yaml     // For ShardingSphere-JDBC, it's for full route with encrypt rule.
│   │    │   │   └───masterslave
│   │    │   │   │   └───proxy
│   │    │   │   │   │ config-proxy-fullrouting-masterslave.yaml        // For ShardingSphere-Proxy, it's for full route with master-slave rule.
│   │    │   │   │   └───shardingjdbc
│   │    │   │   │   │ config-shardingjdbc-fullrouting-masterslave.yaml // For ShardingSphere-JDBC, it's for full route with master-slave rule.
│   │    │   │   └───sharding
│   │    │   │   │   └───proxy
│   │    │   │   │   │ config-proxy-fullrouting-sharding.yaml           // For ShardingSphere-Proxy, it's for full route with sharding rule.
│   │    │   │   │   └───shardingjdbc
│   │    │   │   │   │ config-shardingjdbc-fullrouting-sharding.yaml    // For ShardingSphere-JDBC, it's for full route with sharding rule.
│   │    │   │   └───sharding-masterslave-encrypt
│   │    │   │   │   └───proxy
│   │    │   │   │   │ config-proxy-fullrouting-sharding-masterslave-enc.yaml             // For ShardingSphere-Proxy, it's for full route with sharding-masterslave-enrypt rule.
│   │    │   │   │   └───shardingjdbc
│   │    │   │   │   │ config-shardingjdbc-fullrouting-sharding-masterslave-encrypt.yaml  // For ShardingSphere-JDBC, it's for full route with sharding-masterslave-enrypt rule.
│   │    │   └───rangerouting             // Range routine scenario, file names of its yaml are similar with ones under *fullrouting* directory, please refer to them. 
│   │    │   │
│   │    │   └───singlerouting            // Single routine scenario, file names of its yaml are similar with ones under *fullrouting* directory, please refer to them. 
```

## How to work
ShardingSphere Benchmark is mainly achieved by JMeter in summary, running as command *jmeter -n -t testplan*.

### Environment Preparation
Due to the diversity of user system environment, we don't provide an unified installation mode for automatical deployment, please install them below manually. You could skip it if having them.

#### Install MYSQL 
* Get [MYSQL installation](https://dev.mysql.com/downloads/mysql/) from official website depending on the system you use. It's appreciated to choose MYSQL version above 5.
* Install and set up MYSQL according to its [MYSQL Doc](https://dev.mysql.com/doc), please choose right version of MYSQL doc in compliance with your installation.

#### Install JDK
* Get [JDK installation](https://www.oracle.com/java/technologies/javase-downloads.html) from official website depending on the system you use. It's appreciated to choose JDK version above 7.
* Install and set up JDK according to *Installation Instructions* [JDK installation](https://www.oracle.com/java/technologies/javase-downloads.html).

#### Install Maven
* Get [Maven installation](http://maven.apache.org/download.cgi) from official website depending on the system you use. It's appreciated to choose MYSQL version 3.6.
* Install ans set up Maven according to [Maven doc](http://maven.apache.org/install.html).

#### Get JDBC Jar
* Get JDBC Jar from official website [JDBC Jar url](https://dev.mysql.com/downloads/connector/j/). Its version depends on MYSQL you use.

#### Build ShardingSphere Benchmark
* Get source code of ShardingSphere from [ShardingSphere Benchmark Code URL](https://github.com/wcsoft/shardingsphere-benchmark).
* Build ShardingSphere Benchmark project by running command below.
```bash
mvn clean install
```

#### Install ShardingSphere-Proxy
* Get source code of ShardingSphere from [ShardingSphere Source Code URL](https://github.com/apache/shardingsphere), the features for different branches refer to [ShardingSphere Doc](https://shardingsphere.apache.org/).
* Get ShardingSphere-Proxy installation by running command below. For ShardingSphere 4.x, it's located at *{ShardingSphere_project_base_dir}/sharding-distribution/sharding-proxy-distribution/target*; for ShardingSphere 5.x, it's located at *{project_base_dir}/shardingsphere-distribution/shardingsphere-proxy-distribution/target*.
```bash
mvn clean install -Prelease
```
* Decompress ShardingSphere-proxy above, delete yaml at *{ShardingSphere-Proxy_base_dir}/conf*, copy *server.yaml* and *config-proxy-\*.yaml* which is corresponding test scenarios. These yaml are at *{ShardingSphere Benchmark base dir}/src/main/resources/yaml*.
* Leave ShardingSphere Benchmark jar and JDBC jar into *{ShardingSphere-Proxy_base_dir}/lib*.
* Start ShardingSphere-Proxy, startup scripts are at *{ShardingSphere-Proxy_base_dir}/bin*, choose the right script according to your system.


#### Install JMeter
* Get [JMeter installation](https://jmeter.apache.org/download_jmeter.cgi) from official website depending on the JDK you use. It's appreciated to choose JMeter version 5.3.
* Install and set up JMeter according to [JMeter doc](https://jmeter.apache.org/usermanual/get-started.html).
* Leave ShardingSphere Benchmark jar into *{JMeter_base_dir}/lib/ext*.


#### Configuration
We have extracted necessary user configuration which is in *{ShardingSphere Benchmark base dir}/src/main/resources/config/user-config.properties*. You have to change configuration list below as yours. By default we configure any ip with host in the code or yaml to prevent ip leakage and cause security problems。 We provide the unified host configuration at *{ShardingSphere Benchmark base dir}/src/main/resources/config/baitiao_test_machline_hosts*, modify ips as yours and replace the host file. There is no need to change those ips if running benchmark at local machine.

```bash
// It is base directory of ShardingSphere project.
shardingsphere.project.base.path=/export/jenkins/workspace/ShardingSphere-Benchmark-Deploy
// It is base directory of ShardingSphere benchmark project.
shardingsphere.benchmark.project.base.path=/export/benchmark/shardingsphere-benchmark
// It is base directory of ShardingSphere benchmark result.
shardingsphere.benchmark.result.base.path=/export/shardingsphere-benchmark/result
// It is database port of ShardingSphere benchmark result accessing.
shardingsphere.benchmark.result.database.port=3306
// It is database username of ShardingSphere benchmark result using.
shardingsphere.benchmark.result.database.username=root
// It is database password of ShardingSphere benchmark result, we can leave it empty by default.
shardingsphere.benchmark.result.database.password=
// It is host or ip list of ShardingSphere benchmark using.
shardingsphere.benchmark.database.machine.host.list=ss.benchmark.fullrouting.encrypt.ds0;ss.benchmark.fullrouting.encrypt.ds1;ss.benchmark.fullrouting.encrypt.ds2
// It is database host of ShardingSphere benchmark result using. Statistics of ShardingSphere benchmark will be stored into the database. 
shardingsphere.benchmark.result.database.host=ss.benchmark.result
```

#### Init Database/Table
We have to prepare database and table for benchmark. They are configured at *{ShardingSphere Benchmark base dir}/src/main/resources/config/user-config.properties*, run the command below to create physical databases and tables.
```bash
jmeter -n -t {ShardingSphere Benchmark base dir}/src/main/resources/testplan/common/initdb/initdb-testplan.jmx
```

#### Init Base Data
All of initing base data test plans for different scenarios are at *{ShardingSphere Benchmark base dir}/src/main/resources/testplan/common/datapreparation*. Here is a command for instance, which scenario to test in final depends on your choice.  
```bash
jmeter -n -t {ShardingSphere Benchmark base dir}/src/main/resources/testplan/common/datapreparation/shardingsphere/sharding/shardingsphere-sharding-createdata-testplan.jmx
```

#### Run a Test Plan
All of test plans for different scenarios are at *{ShardingSphere Benchmark base dir}/src/main/resources/testplan/*. Here is a command for instance, which scenario to test in final depends on your choice.  
```bash
jmeter -n -t {ShardingSphere Benchmark base dir}/src/main/resources/testplan/singlerouting/encrypt/shardingjdbc-singlerouting-encrypt-select-testplan.jmx
```

After running the command successfully, it generates a JMeter result file located at *shardingsphere.benchmark.result.base.path* which is configured in *{ShardingSphere Benchmark base dir}/src/main/resources/config/user-config.properties*. The JMeter result looks like the figure below.
```bash
timeStamp,elapsed,label,responseCode,responseMessage,threadName,dataType,success,failureMessage,bytes,sentBytes,grpThreads,allThreads,Latency,IdleTime,Connect
1598274390236,8657,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-74,,true,,0,0,100,100,0,0,0
1598274390236,8657,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-65,,true,,0,0,100,100,0,0,0
1598274390236,8878,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-42,,true,,0,0,100,100,0,0,0
1598274390234,8917,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-15,,true,,0,0,100,100,0,0,0
1598274390234,9002,JMeterShardingJDBCFullRoutingEncryptSelect,,,Thread Group 1-3,,true,,0,0,100,100,0,0,0
```

#### Statistics
We could calculate the general statistics by JMeter result such as TPS. There are two physical tables to store the statistics, *benchmark_result* and *benchmark_avg_result*. You could query the result from the databases directly as well if you prefer. Trigger the sql below to get latest record for the test.

* Physical table *benchmark_result*: intermediate result will be here, its table structure is below.

|       *Field*       |  *Description*           |   
| ----------------------- | --------------------- |
| id              |  Primary key generated automatically.  | 
| product         |  Tested product that is ShardingJDBC, ShardingProxy or MYSQL. | 
| version         |  ShardingSphere version configured at user-config.properties.                | 
| scenario        |  Three kinds for benchmark including FullRouting, RangeRouting, SingleRouting. | 
| rules           |  Four kinds of rules for benchmark including Encrypt, MasterSlave, Sharding, Sharding+Master+Slave+Encrypt. | 
| tps             |  Tps which is got from JMeter file, the formula is *total count / total time*. | 
| total           |  Sample count which is taken out the head data and tail data.                | 
| maxCost         |  Max time cost of running every sql.                | 
| minCost         |  Min time cost of running every sql. | 
| dbsql           |  Actual sql to run.                | 
| dboperation     |  Actual sql type to run including Select and Insert+Update+Delete. | 
| concurrency     |  Count of concurrency configured at user-config.properties.                | 
| tableshardingcount|  Table sharding count configured at user-config.properties, used at yaml.                 | 
| dbshardingcount  |  Db sharding count configured at user-config.properties, used at yaml.                | 


* Physical table *benchmark_avg_result*: average result will be here, its table structure is below.

|       *Field*       |  *Description*           |   
| ----------------------- | --------------------- |
| id              |  Primary key generated automatically.  | 
| product         |  Tested product that is ShardingJDBC, ShardingProxy or MYSQL. | 
| version         |  ShardingSphere version configured at user-config.properties.                | 
| scenario        |  Three kinds for benchmark including FullRouting, RangeRouting, SingleRouting. | 
| rules           |  Four kinds of rules for benchmark including Encrypt, MasterSlave, Sharding, Sharding+Master+Slave+Encrypt. | 
| avg_tps         |  Average tps for the same test, the formula is *total tps / total count*. | 
| total           |  Sample count which is taken out the head data and tail data.                | 
| maxCost         |  Max time cost of running every sql.                | 
| minCost         |  Min time cost of running every sql. | 
| dbsql           |  Actual sql to run.                | 
| dboperation     |  Actual sql type to run including Select and Insert+Update+Delete. | 
| concurrency     |  Count of concurrency configured at user-config.properties.                | 
| tableshardingcount|  Table sharding count configured at user-config.properties, used at yaml.                 | 
| dbshardingcount  |  Db sharding count configured at user-config.properties, used at yaml.                | 

Run the command to get statistics.
```bash
jmeter -n -t {ShardingSphere Benchmark base dir}/src/main/resources/testplan/statistic/ss-benchmark-statistic-testplan.jmx
```

```bash
select * from benchmark_result order by id limit 1
select * from benchmark_avg_result order by id limit 1
```
## SharingSphere 的 sysbench 压测工具集


### Sysbench 的简介

Sysbench 是一款基于 LuaJIT 的开源脚本化基准测试工具集，常用于测试 CPU、内存、I/O 的性能。自带的一系列脚本可以有针对性的测试 OLTP 类数据库的性能。

ShardingSphere 引入 Sysbench 作为性能测试工具集的一部分，可以通过 Sysbench 测试 proxy 连接 MySQL、PGSQL 等的性能，并与直连 MySQL、PGSQL 等数据库进行对比。

### Sysbench 测试所需的环境

目前 ShardingSphere 是通过 Jenkins 每日定时触发任务进行压测的，使用的硬件如下：

| 主机      | CPU     | 内存 | IP |
| ------   | ------  | ---  | --- |
| Jenkins  | 4 core  | 8G   | 10.0.100.10 |
| Sysbench | 8 core  | 16G  | 10.0.100.20 |
| Proxy    | 32 core | 32G | 10.0.100.30 |
| MySQL or PGSQL | 32 core | 32G  | 10.0.100.40 |
| MySQL or PGSQL | 32 core | 32G  | 10.0.100.41 |

网络带宽统一为 10Gb Ethernet

![](../resources/image/sysbench-distributed-arch.png)

假设目前有 5 台主机，ip 以及配置如上图

在 `10.0.100.10` 上安装 Jenkins，并设置两个 node。

在 `10.0.100.20` 上安装 Sysbench 并分别在 `10.0.100.20`，`10.0.100.30` 启动 Jenkins Master 上下载的 agent。


### Jenkins 的 Pipeline 是如何组成的

Sysbench 是不依赖 Jenkins 以及任何 CI/CD 工具的，甚至可以手动在命令行中执行 sysbench 的脚本。选用 Jenkins 的目的是利用 Jenkins 的 pipeline 管理压测流程，使之透明化、自动化。

如下为 Jenkins 管理压测步骤的流程：

  1. 安装 Proxy
  2. 准备 sysbench
  3. 准备 proxy
  4. 测试 proxy

#### 安装 proxy

在这一阶段，Jenkins 会通过 agent 创建 proxy 所需目录，通过 git 克隆 ShardingSphere 的代码，编译 ShardingSphere 的代码，解压缩编译后的 proxy districution

#### 准备 sysbench

在这一阶段，Jenkins 会通过 agent 创建 sysbench 结果集所需要的目录，准备好 sysbench 所需的脚本跟参数

#### 准备 proxy
 
在这一阶段，Jenkins 会将 proxy 所需的配置文件跟驱动准备好（例如 MySQL Driver），复制到 proxy 中的相应目录并且启动 proxy。

#### 测试 proxy

在这一阶段，Jenkins 会根据配置文件中的参数启动 sysbench 压测 proxy。压测结束后，会通过一个 Python 脚本将不同批次的压测结果整合并输出为图片，在 Jenkins 的 report 中即可查看不同批次的结果变化。

### sysbench 的 report

sysbench 的结果会输出为一个 .txt 文件，内容类似如下：

```
SQL statistics:
    queries performed:
        read:                            28147663
        write:                           0
        other:                           0
        total:                           28147663
    transactions:                        28147663 (156336.98 per sec.)
    queries:                             28147663 (156336.98 per sec.)
    ignored errors:                      0      (0.00 per sec.)
    reconnects:                          0      (0.00 per sec.)

General statistics:
    total time:                          180.0437s
    total number of events:              28147663

Latency (ms):
         min:                                    0.59
         avg:                                    2.46
         max:                                  281.08
         99th percentile:                        5.28
         sum:                             69103716.45

Threads fairness:
    events (avg/stddev):           73301.2057/786.12
    execution time (avg/stddev):   179.9576/0.01
```

项目中的 python 脚本会根据 sysbench 生成如下的统计图表：

![](../resources/image/sysbench_result_img.png)

### 如何自己通过 Jenkins 做同样的测试

建议 fork 一份当前项目，然后创建 Jenkins 的 pipeline，`Pipeline` 部分选择 `pipeline script from SCM`。SCM 的 git 地址为当前项目地址 `https://github.com/apache/shardingsphere-benchmark.git`

想测试什么类型的数据库组合，根据不同的 script path 进行设置：

测试 mysql : sysbench/mysql/sysbench-mysql.pipeline
测试 pgsql : sysbench/mysql/sysbench-pgsql.pipeline
测试 proxy + mysql : sysbench/proxy-mysql/sysbench-proxy-mysql.pipeline
测试 proxy + pgsqsl : sysbench/proxy-pgsql/sysbench-proxy-pgsql.pipeline

同时修改 `.env` 文件以及 `prepared-conf` 中对应的 yaml 配置，以期符合自己的测试需求
> 如果需要测试 mysql，需要自行将 MySQL 的 driver(例如 mysql-connector-java-8.0.24.jar) 放入要测试的目录的 prepared-conf 下。
如下图所示 : 

![](../resources/image/jenkins-sysbench-pipeline.png)
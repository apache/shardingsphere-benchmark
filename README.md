# incubator-shardingsphere-benchmark
## shadingsphere-benchmark-ui
### Builds setup
```bash

shadingsphere-benchmark-ui/README.md

```
## shardingsphere-benchmark
### Builds setup
```bash

step 1: build shardingsphere-benchmark
# Notes: this tool depend on the master of incubator-shardingsphere, you should compile the master of incubator-shardingsphere first if the version of master has not been released.
# mvn clean install

step 2: copy target jar to jmeter lib/ext
# cp shardingsphere-benchmark-1.0-SNAPSHOT-jar-with-dependencies.jar  apache-jmeter-**/lib/ext

step 3: run jmeter test plan with target class


```

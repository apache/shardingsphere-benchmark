export JAVA_HOME=/export/jdk1.8.0_192/
export CLASSPATH=.:$JAVA_HOME/jre/lib/rt.jar:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar
export PATH=$PATH:$JAVA_HOME/bin
cd /export/benchmark/sjperformance
/export/apache-maven-3.6.0/bin/mvn clean install
#scp -r target/*-1.0-SNAPSHOT.jar jenkins@10.222.16.98:/home/jenkins/apache-jmeter-4.0/lib/ext
scp -r target/*-jar-with-dependencies.jar jenkins@10.222.16.98:/home/jenkins/apache-jmeter-4.0/lib/ext
cd /export/benchmark/shardingsphere-benchmark
/export/apache-maven-3.6.0/bin/mvn clean install
#scp -r target/*-1.0-SNAPSHOT.jar jenkins@10.222.16.98:/home/jenkins/apache-jmeter-4.0/lib/ext
scp -r target/shardingsphere-benchmark-1.1-SNAPSHOT.jar jenkins@10.222.16.98:/home/jenkins/apache-jmeter-4.0/lib/ext
#scp -r target/*-jar-with-dependencies.jar jenkins@10.222.16.98:/home/jenkins/apache-jmeter-4.0/lib/ext

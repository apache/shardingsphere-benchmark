package org.apache.shardingsphere.benchmark.common.file.jmeter;

import org.apache.shardingsphere.benchmark.common.file.util.FileUtil;

import java.io.File;
import java.util.Map;

public final class BenchmarkResultDirManagement {
    
    /**
     * Manage benchmark result directory of jtl.
     * @param userConfig
     */
    public static void manageResultDir(Map userConfig){
        String benchmarkResultPath = (String)userConfig.get("shardingsphere.benchmark.result.base.path");
        String benchmarkResultFullRoutingEncryptPath = benchmarkResultPath + "/fullrouting/encrypt";
        String benchmarkResultFullRoutingMasterSlavePath = benchmarkResultPath + "/fullrouting/masterslave";
        String benchmarkResultFullRoutingShardingPath = benchmarkResultPath + "/fullrouting/sharding";
        String benchmarkResultFullRoutingShardingMasterSlaveEncryptPath = benchmarkResultPath + "/fullrouting/shardingmasterslaveencrypt";
        String benchmarkResultRangeRoutingEncryptPath = benchmarkResultPath + "/rangerouting/encrypt";
        String benchmarkResultRangeRoutingMasterSlavePath = benchmarkResultPath + "/rangerouting/masterslave";
        String benchmarkResultRangeRoutingShardingPath = benchmarkResultPath + "/rangerouting/sharding";
        String benchmarkResultRangeRoutingShardingMasterSlaveEncryptPath = benchmarkResultPath + "/rangerouting/shardingmasterslaveencrypt";
        String benchmarkResultSingleRoutingEncryptPath = benchmarkResultPath + "/singlerouting/encrypt";
        String benchmarkResultSingleRoutingMasterSlavePath = benchmarkResultPath + "/singlerouting/masterslave";
        String benchmarkResultSingleRoutingShardingPath = benchmarkResultPath + "/singlerouting/sharding";
        String benchmarkResultSingleRoutingShardingMasterSlaveEncryptPath = benchmarkResultPath + "/singlerouting/shardingmasterslaveencrypt";
        manageEachScenarioResultDir(benchmarkResultFullRoutingEncryptPath);
        manageEachScenarioResultDir(benchmarkResultFullRoutingMasterSlavePath);
        manageEachScenarioResultDir(benchmarkResultFullRoutingShardingPath);
        manageEachScenarioResultDir(benchmarkResultFullRoutingShardingMasterSlaveEncryptPath);
        manageEachScenarioResultDir(benchmarkResultRangeRoutingEncryptPath);
        manageEachScenarioResultDir(benchmarkResultRangeRoutingMasterSlavePath);
        manageEachScenarioResultDir(benchmarkResultRangeRoutingShardingPath);
        manageEachScenarioResultDir(benchmarkResultRangeRoutingShardingMasterSlaveEncryptPath);
        manageEachScenarioResultDir(benchmarkResultSingleRoutingEncryptPath);
        manageEachScenarioResultDir(benchmarkResultSingleRoutingMasterSlavePath);
        manageEachScenarioResultDir(benchmarkResultSingleRoutingShardingPath);
        manageEachScenarioResultDir(benchmarkResultSingleRoutingShardingMasterSlaveEncryptPath);
    }
    
    /**
     * Manage each scenario result directory.
     * @param path
     */
    public static void manageEachScenarioResultDir(String path){
        if (false == FileUtil.isExisted(path)){
            FileUtil.createDirs(path);
        } else {
            FileUtil.deleteFile(new File(path));
        }
    }
}

package org.apache.shardingsphere.benchmark.common.statistic;

import org.apache.shardingsphere.benchmark.bean.BenchmarkResultBean;
import org.apache.shardingsphere.benchmark.common.file.jmeter.BenchmarkResultParser;

import javax.sql.DataSource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Benchmark range routing statistic.
 */
public class BenchmarkRangeroutingStatistic extends BenchmarkStatistic{
    
    public List<BenchmarkResultBean> result = new ArrayList<BenchmarkResultBean>(10);
    
    /**
     * Calculate result for range routing scenario.
     * 
     * @param benchmarkResultPath
     * @param sqlConfig
     * @param benchmarkVersion
     * @param skipBegin
     * @param skipEnd
     * @param concurrency
     * @param updateTime
     * @param dbShardingCount
     * @param tableShardingCount
     * @return
     */
    public List<BenchmarkResultBean> calculateRangeRoutingScenarioResult(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        calculateRangeroutingEncrypt(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        calculateRangeRoutingMasterslave(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        calculateRangeRoutingSharding(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        calculateRangeRoutingShardingMasterslaveEncrypt(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        return result;
    }
    
    /**
     * Calculate result for range routing encrypt scenario.
     * 
     * @param benchmarkResultPath
     * @param sqlConfig
     * @param benchmarkVersion
     * @param skipBegin
     * @param skipEnd
     * @param concurrency
     * @param updateTime
     * @param dbShardingCount
     * @param tableShardingCount
     */
    public void calculateRangeroutingEncrypt(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        String proxyRangeRoutingEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.rangerouting.encrypt.insertupdatedelete.result");
        Map proxyRangeRoutingEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(proxyRangeRoutingEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (proxyRangeRoutingEncryptInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.rangerouting.encrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.encrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.encrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean proxyRangeRoutingEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion,proxyRangeRoutingEncryptInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Encrypt", "ShardingProxy", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(proxyRangeRoutingEncryptInsertupdatedeleteResultBean);
        }
        String shardingjdbcRangeRoutingEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.rangerouting.encrypt.insertupdatedelete.result");
        Map shardingjdbcRangeRoutingEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcRangeRoutingEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (shardingjdbcRangeRoutingEncryptInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.rangerouting.encrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.encrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.encrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean shardingjdbcRangeRoutingEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcRangeRoutingEncryptInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Encrypt", "ShardingJDBC", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(shardingjdbcRangeRoutingEncryptInsertupdatedeleteResultBean);
        }
        String jdbcRangeRoutingEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.rangerouting.encrypt.insertupdatedelete.result");
        Map jdbcRangeRoutingEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(jdbcRangeRoutingEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (jdbcRangeRoutingEncryptInsertupdatedeleteResult.size() > 0){
            String jdbcInsertUpdateDeleteSQL = (String)sqlConfig.get("jdbc.benchmark.rangerouting.encrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.rangerouting.encrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.rangerouting.encrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean jdbcRangeRoutingEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcRangeRoutingEncryptInsertupdatedeleteResult, jdbcInsertUpdateDeleteSQL, "Encrypt", "MYSQL", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(jdbcRangeRoutingEncryptInsertupdatedeleteResultBean);
        }
        String proxyRangeRoutingEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.rangerouting.encrypt.select.result");
        Map proxyRangeRoutingEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(proxyRangeRoutingEncryptSelectResultPath, skipBegin, skipEnd);
        if (proxyRangeRoutingEncryptSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.rangerouting.encrypt.select.sql");
            BenchmarkResultBean proxyRangeRoutingEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, proxyRangeRoutingEncryptSelectResult, ssSelectSql, "Encrypt", "ShardingProxy", "RangeRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(proxyRangeRoutingEncryptSelectResultBean);
        }
        String shardingjdbcRangeRoutingEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.rangerouting.encrypt.select.result");
        Map shardingjdbcRangeRoutingEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcRangeRoutingEncryptSelectResultPath, skipBegin, skipEnd);
        if (shardingjdbcRangeRoutingEncryptSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.rangerouting.encrypt.select.sql");
            BenchmarkResultBean shardingjdbcRangeRoutingEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcRangeRoutingEncryptSelectResult, ssSelectSql, "Encrypt", "ShardingJDBC", "RangeRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(shardingjdbcRangeRoutingEncryptSelectResultBean);
        }
        String jdbcRangeRoutingEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.rangerouting.encrypt.select.result");
        Map jdbcRangeRoutingEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(jdbcRangeRoutingEncryptSelectResultPath, skipBegin, skipEnd);
        if (jdbcRangeRoutingEncryptSelectResult.size() > 0) {
            String jdbcSelectSql = (String)sqlConfig.get("jdbc.benchmark.rangerouting.encrypt.select.sql");
            BenchmarkResultBean jdbcRangeRoutingEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcRangeRoutingEncryptSelectResult, jdbcSelectSql, "Encrypt", "MYSQL", "RangeRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(jdbcRangeRoutingEncryptSelectResultBean);
        }
    }
    
    /**
     * Calculate result for range routing master-slave scenario.
     * 
     * @param benchmarkResultPath
     * @param sqlConfig
     * @param benchmarkVersion
     * @param skipBegin
     * @param skipEnd
     * @param concurrency
     * @param updateTime
     * @param dbShardingCount
     * @param tableShardingCount
     */
    public void calculateRangeRoutingMasterslave(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        String proxyRangeRoutingMasterSlaveInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.rangerouting.masterslave.insertupdatedelete.result");
        Map proxyRangeRoutingMasterSlaveInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(proxyRangeRoutingMasterSlaveInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (proxyRangeRoutingMasterSlaveInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.rangerouting.masterslave.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.masterslave.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.masterslave.update.sql") +  ";\r\n";
            BenchmarkResultBean proxyRangeRoutingMasterSlaveInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion,proxyRangeRoutingMasterSlaveInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "MasterSlave", "ShardingProxy", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(proxyRangeRoutingMasterSlaveInsertupdatedeleteResultBean);
        }
        String shardingjdbcRangeRoutingMasterSlaveInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.rangerouting.masterslave.insertupdatedelete.result");
        Map shardingjdbcRangeRoutingMasterSlaveInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcRangeRoutingMasterSlaveInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (shardingjdbcRangeRoutingMasterSlaveInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.rangerouting.masterslave.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.masterslave.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.masterslave.update.sql") +  ";\r\n";
            BenchmarkResultBean shardingjdbcRangeRoutingMasterSlaveInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcRangeRoutingMasterSlaveInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "MasterSlave", "ShardingJDBC", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(shardingjdbcRangeRoutingMasterSlaveInsertupdatedeleteResultBean);
        }
        String jdbcRangeRoutingMasterSlaveInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.rangerouting.masterslave.insertupdatedelete.result");
        Map jdbcRangeRoutingMasterSlaveInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(jdbcRangeRoutingMasterSlaveInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (jdbcRangeRoutingMasterSlaveInsertupdatedeleteResult.size() > 0) {
            String jdbcInsertUpdateDeleteSQL = (String)sqlConfig.get("jdbc.benchmark.rangerouting.masterslave.delete.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.rangerouting.masterslave.insert.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.rangerouting.masterslave.update.sql") +  ";\r\n";
            BenchmarkResultBean jdbcRangeRoutingMasterSlaveInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcRangeRoutingMasterSlaveInsertupdatedeleteResult, jdbcInsertUpdateDeleteSQL, "MasterSlave", "MYSQL", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(jdbcRangeRoutingMasterSlaveInsertupdatedeleteResultBean);
        }
        String proxyRangeRoutingMasterSlaveSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.rangerouting.masterslave.select.result");
        Map proxyRangeRoutingMasterSlaveSelectResult = BenchmarkResultParser.benchmarkStatistic(proxyRangeRoutingMasterSlaveSelectResultPath, skipBegin, skipEnd);
        if (proxyRangeRoutingMasterSlaveSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.rangerouting.masterslave.select.sql");
            BenchmarkResultBean proxyRangeRoutingMasterSlaveSelectResultBean = new BenchmarkResultBean(benchmarkVersion, proxyRangeRoutingMasterSlaveSelectResult, ssSelectSql, "MasterSlave", "ShardingProxy", "RangeRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(proxyRangeRoutingMasterSlaveSelectResultBean);
        }
        String shardingjdbcRangeRoutingMasterSlaveSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.rangerouting.masterslave.select.result");
        Map shardingjdbcRangeRoutingMasterSlaveSelectResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcRangeRoutingMasterSlaveSelectResultPath, skipBegin, skipEnd);
        if (shardingjdbcRangeRoutingMasterSlaveSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.rangerouting.masterslave.select.sql");
            BenchmarkResultBean shardingjdbcRangeRoutingMasterSlaveSelectResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcRangeRoutingMasterSlaveSelectResult, ssSelectSql, "MasterSlave", "ShardingJDBC", "RangeRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(shardingjdbcRangeRoutingMasterSlaveSelectResultBean);
        }
        String jdbcRangeRoutingMasterSlaveSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.rangerouting.masterslave.select.result");
        Map jdbcRangeRoutingMasterSlaveSelectResult = BenchmarkResultParser.benchmarkStatistic(jdbcRangeRoutingMasterSlaveSelectResultPath, skipBegin, skipEnd);
        if (jdbcRangeRoutingMasterSlaveSelectResult.size() > 0) {
            String jdbcSelectSql = (String)sqlConfig.get("jdbc.benchmark.rangerouting.masterslave.select.sql");
            BenchmarkResultBean jdbcRangeRoutingMasterSlaveSelectResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcRangeRoutingMasterSlaveSelectResult, jdbcSelectSql, "MasterSlave", "MYSQL", "RangeRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(jdbcRangeRoutingMasterSlaveSelectResultBean);
        }
    }
    
    /**
     * Calculate result for range routing sharding scenario.
     * 
     * @param benchmarkResultPath
     * @param sqlConfig
     * @param benchmarkVersion
     * @param skipBegin
     * @param skipEnd
     * @param concurrency
     * @param updateTime
     * @param dbShardingCount
     * @param tableShardingCount
     */
    public void calculateRangeRoutingSharding(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        String proxyRangeRoutingShardingInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.rangerouting.sharding.insertupdatedelete.result");
        Map proxyRangeRoutingShardingInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(proxyRangeRoutingShardingInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (proxyRangeRoutingShardingInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.rangerouting.sharding.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.sharding.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.sharding.update.sql") +  ";\r\n";
            BenchmarkResultBean proxyRangeRoutingShardingInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion,proxyRangeRoutingShardingInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Sharding", "ShardingProxy", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(proxyRangeRoutingShardingInsertupdatedeleteResultBean);
        }
        String shardingjdbcRangeRoutingShardingInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.rangerouting.sharding.insertupdatedelete.result");
        Map shardingjdbcRangeRoutingShardingInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcRangeRoutingShardingInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (shardingjdbcRangeRoutingShardingInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.rangerouting.sharding.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.sharding.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.sharding.update.sql") +  ";\r\n";
            BenchmarkResultBean shardingjdbcRangeRoutingShardingInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcRangeRoutingShardingInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Sharding", "ShardingJDBC", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(shardingjdbcRangeRoutingShardingInsertupdatedeleteResultBean);
        }
        String jdbcRangeRoutingShardingInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.rangerouting.sharding.insertupdatedelete.result");
        Map jdbcRangeRoutingShardingInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(jdbcRangeRoutingShardingInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (jdbcRangeRoutingShardingInsertupdatedeleteResult.size() > 0) {
            String jdbcInsertUpdateDeleteSQL = (String)sqlConfig.get("jdbc.benchmark.rangerouting.sharding.delete.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.rangerouting.sharding.insert.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.rangerouting.sharding.update.sql") +  ";\r\n";
            BenchmarkResultBean jdbcRangeRoutingShardingInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcRangeRoutingShardingInsertupdatedeleteResult, jdbcInsertUpdateDeleteSQL, "Sharding", "MYSQL", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(jdbcRangeRoutingShardingInsertupdatedeleteResultBean);
        }
        String proxyRangeRoutingShardingSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.rangerouting.sharding.select.result");
        Map proxyRangeRoutingShardingSelectResult = BenchmarkResultParser.benchmarkStatistic(proxyRangeRoutingShardingSelectResultPath, skipBegin, skipEnd);
        if (proxyRangeRoutingShardingSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.rangerouting.sharding.select.sql");
            BenchmarkResultBean proxyRangeRoutingShardingSelectResultBean = new BenchmarkResultBean(benchmarkVersion, proxyRangeRoutingShardingSelectResult, ssSelectSql, "Sharding", "ShardingProxy", "RangeRouting", "Select", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(proxyRangeRoutingShardingSelectResultBean);
        }
        String shardingjdbcRangeRoutingShardingSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.rangerouting.sharding.select.result");
        Map shardingjdbcRangeRoutingShardingSelectResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcRangeRoutingShardingSelectResultPath, skipBegin, skipEnd);
        if (shardingjdbcRangeRoutingShardingSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.rangerouting.sharding.select.sql");
            BenchmarkResultBean shardingjdbcRangeRoutingShardingSelectResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcRangeRoutingShardingSelectResult, ssSelectSql, "Sharding", "ShardingJDBC", "RangeRouting", "Select", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(shardingjdbcRangeRoutingShardingSelectResultBean);
        }
        String jdbcRangeRoutingShardingSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.rangerouting.sharding.select.result");
        Map jdbcRangeRoutingShardingSelectResult = BenchmarkResultParser.benchmarkStatistic(jdbcRangeRoutingShardingSelectResultPath, skipBegin, skipEnd);
        if (jdbcRangeRoutingShardingSelectResult.size() > 0) {
            String jdbcSelectSql = (String)sqlConfig.get("jdbc.benchmark.rangerouting.sharding.select.sql");
            BenchmarkResultBean jdbcRangeRoutingShardingSelectResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcRangeRoutingShardingSelectResult, jdbcSelectSql, "Sharding", "MYSQL", "RangeRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(jdbcRangeRoutingShardingSelectResultBean);
        }
    }
    
    /**
     * Calculate result for range routing sharding-master-slave-encrypt scenario.
     * 
     * @param benchmarkResultPath
     * @param sqlConfig
     * @param benchmarkVersion
     * @param skipBegin
     * @param skipEnd
     * @param concurrency
     * @param updateTime
     * @param dbShardingCount
     * @param tableShardingCount
     */
    public void calculateRangeRoutingShardingMasterslaveEncrypt(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        String proxyRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.rangerouting.shardingmasterslaveencrypt.insertupdatedelete.result");
        Map proxyRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(proxyRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (proxyRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean proxyRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion,proxyRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Sharding+Master+Slave+Encrypt", "ShardingProxy", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(proxyRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean);
        }
        String shardingjdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.rangerouting.shardingmasterslaveencrypt.insertupdatedelete.result");
        Map shardingjdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (shardingjdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean shardingjdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Sharding+Master+Slave+Encrypt", "ShardingJDBC", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(shardingjdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean);
        }
        String jdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.rangerouting.shardingmasterslaveencrypt.insertupdatedelete.result");
        Map jdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(jdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (jdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult.size() > 0) {
            String jdbcInsertUpdateDeleteSQL = (String)sqlConfig.get("jdbc.benchmark.rangerouting.shardingmasterslaveencrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.rangerouting.shardingmasterslaveencrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.rangerouting.shardingmasterslaveencrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean jdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult, jdbcInsertUpdateDeleteSQL, "Sharding+Master+Slave+Encrypt", "MYSQL", "RangeRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(jdbcRangeRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean);
        }
        String proxyRangeRoutingShardingMasterSlaveEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.rangerouting.shardingmasterslaveencrypt.select.result");
        Map proxyRangeRoutingShardingMasterSlaveEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(proxyRangeRoutingShardingMasterSlaveEncryptSelectResultPath, skipBegin, skipEnd);
        if (proxyRangeRoutingShardingMasterSlaveEncryptSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.select.sql");
            BenchmarkResultBean proxyRangeRoutingShardingMasterSlaveEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, proxyRangeRoutingShardingMasterSlaveEncryptSelectResult, ssSelectSql, "Sharding+Master+Slave+Encrypt", "ShardingProxy", "RangeRouting", "Select", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(proxyRangeRoutingShardingMasterSlaveEncryptSelectResultBean);
        }
        String shardingjdbcRangeRoutingShardingMasterSlaveEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.rangerouting.shardingmasterslaveencrypt.select.result");
        Map shardingjdbcRangeRoutingShardingMasterSlaveEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcRangeRoutingShardingMasterSlaveEncryptSelectResultPath, skipBegin, skipEnd);
        if (shardingjdbcRangeRoutingShardingMasterSlaveEncryptSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.select.sql");
            BenchmarkResultBean shardingjdbcRangeRoutingShardingMasterSlaveEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcRangeRoutingShardingMasterSlaveEncryptSelectResult, ssSelectSql, "Sharding+Master+Slave+Encrypt", "ShardingJDBC", "RangeRouting", "Select", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(shardingjdbcRangeRoutingShardingMasterSlaveEncryptSelectResultBean);
        }
        String jdbcRangeRoutingShardingMasterSlaveEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.rangerouting.shardingmasterslaveencrypt.select.result");
        Map jdbcRangeRoutingShardingMasterSlaveEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(jdbcRangeRoutingShardingMasterSlaveEncryptSelectResultPath, skipBegin, skipEnd);
        if (jdbcRangeRoutingShardingMasterSlaveEncryptSelectResult.size() > 0) {
            String jdbcSelectSql = (String)sqlConfig.get("jdbc.benchmark.rangerouting.shardingmasterslaveencrypt.select.sql");
            BenchmarkResultBean jdbcRangeRoutingShardingMasterSlaveEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcRangeRoutingShardingMasterSlaveEncryptSelectResult, jdbcSelectSql, "Sharding+Master+Slave+Encrypt", "MYSQL", "RangeRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(jdbcRangeRoutingShardingMasterSlaveEncryptSelectResultBean);
        }
    }
    
    /**
     * Calculate average result for range routing scenario.
     *
     * @param datasource
     * @param sqlConfig
     * @param noShardingParams
     * @param shardingParams
     * @return
     */
    public  List<BenchmarkResultBean> calculateRangeroutingScenarioAvgResult(DataSource datasource, Map sqlConfig, List noShardingParams, List shardingParams){
        List<BenchmarkResultBean> rangeRoutingCalResult = new ArrayList<BenchmarkResultBean>(10);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.encrypt.shardingjdbc.select.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.encrypt.proxy.select.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.encrypt.mysql.select.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.encrypt.shardingjdbc.insertupdatedelete.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.encrypt.proxy.insertupdatedelete.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.encrypt.mysql.insertupdatedelete.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.masterslave.shardingjdbc.select.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.masterslave.proxy.select.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.masterslave.mysql.select.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.masterslave.shardingjdbc.insertupdatedelete.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.masterslave.proxy.insertupdatedelete.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.masterslave.mysql.insertupdatedelete.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.sharding.shardingjdbc.select.sql"), shardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.sharding.proxy.select.sql"), shardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.sharding.mysql.select.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.sharding.shardingjdbc.insertupdatedelete.sql"), shardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.sharding.proxy.insertupdatedelete.sql"), shardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.sharding.mysql.insertupdatedelete.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.shardingmasterslaveencrypt.shardingjdbc.select.sql"), shardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.shardingmasterslaveencrypt.proxy.select.sql"), shardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.shardingmasterslaveencrypt.mysql.select.sql"), noShardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.shardingmasterslaveencrypt.shardingjdbc.insertupdatedelete.sql"), shardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.shardingmasterslaveencrypt.proxy.insertupdatedelete.sql"), shardingParams, rangeRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.rangerouting.shardingmasterslaveencrypt.mysql.insertupdatedelete.sql"), noShardingParams, rangeRoutingCalResult);
        return rangeRoutingCalResult;
    }
}

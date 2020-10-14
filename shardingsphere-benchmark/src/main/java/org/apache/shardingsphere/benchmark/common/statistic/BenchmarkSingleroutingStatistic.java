package org.apache.shardingsphere.benchmark.common.statistic;

import org.apache.shardingsphere.benchmark.bean.BenchmarkResultBean;
import org.apache.shardingsphere.benchmark.common.file.jmeter.BenchmarkResultParser;

import javax.sql.DataSource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Benchmark single routing statistic.
 */
public class BenchmarkSingleroutingStatistic extends BenchmarkStatistic{
    
    public List<BenchmarkResultBean> result = new ArrayList<BenchmarkResultBean>(10);
    
    /**
     * Calculate result for single routing scenario.
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
    public List<BenchmarkResultBean> calculateSingleRoutingScenarioResult(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        calculateSingleRoutingEncrypt(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        calculateSingleRoutingMasterslave(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        calculateSingleRoutingSharding(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        calculateSingleRoutingShardingMasterslaveEncrypt(benchmarkResultPath, sqlConfig, benchmarkVersion, skipBegin, skipEnd, concurrency, updateTime, dbShardingCount, tableShardingCount);
        return result;
    }
    
    /**
     * Calculate result for single routing encrypt scenario.
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
    public void calculateSingleRoutingEncrypt(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        String proxySingleRoutingEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.singlerouting.encrypt.insertupdatedelete.result");
        Map proxySingleRoutingEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(proxySingleRoutingEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (proxySingleRoutingEncryptInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.singlerouting.encrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.encrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.encrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean proxySingleRoutingEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion,proxySingleRoutingEncryptInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Encrypt", "ShardingProxy", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(proxySingleRoutingEncryptInsertupdatedeleteResultBean);
        }
        String shardingjdbcSingleRoutingEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.singlerouting.encrypt.insertupdatedelete.result");
        Map shardingjdbcSingleRoutingEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcSingleRoutingEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (shardingjdbcSingleRoutingEncryptInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.singlerouting.encrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.encrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.encrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean shardingjdbcSingleRoutingEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcSingleRoutingEncryptInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Encrypt", "ShardingJDBC", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(shardingjdbcSingleRoutingEncryptInsertupdatedeleteResultBean);
        }
        String jdbcSingleRoutingEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.singlerouting.encrypt.insertupdatedelete.result");
        Map jdbcSingleRoutingEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(jdbcSingleRoutingEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (jdbcSingleRoutingEncryptInsertupdatedeleteResult.size() > 0) {
            String jdbcInsertUpdateDeleteSQL = (String)sqlConfig.get("jdbc.benchmark.singlerouting.encrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.singlerouting.encrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.singlerouting.encrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean jdbcSingleRoutingEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcSingleRoutingEncryptInsertupdatedeleteResult, jdbcInsertUpdateDeleteSQL, "Encrypt", "MYSQL", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(jdbcSingleRoutingEncryptInsertupdatedeleteResultBean);
        }
        String proxySingleRoutingEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.singlerouting.encrypt.select.result");
        Map proxySingleRoutingEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(proxySingleRoutingEncryptSelectResultPath, skipBegin, skipEnd);
        if (proxySingleRoutingEncryptSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.singlerouting.encrypt.select.sql");
            BenchmarkResultBean proxySingleRoutingEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, proxySingleRoutingEncryptSelectResult, ssSelectSql, "Encrypt", "ShardingProxy", "SingleRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(proxySingleRoutingEncryptSelectResultBean);
        }
        String shardingjdbcSingleRoutingEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.singlerouting.encrypt.select.result");
        Map shardingjdbcSingleRoutingEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcSingleRoutingEncryptSelectResultPath, skipBegin, skipEnd);
        if (shardingjdbcSingleRoutingEncryptSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.singlerouting.encrypt.select.sql");
            BenchmarkResultBean shardingjdbcSingleRoutingEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcSingleRoutingEncryptSelectResult, ssSelectSql, "Encrypt", "ShardingJDBC", "SingleRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(shardingjdbcSingleRoutingEncryptSelectResultBean);
        }
        String jdbcSingleRoutingEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.singlerouting.encrypt.select.result");
        Map jdbcSingleRoutingEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(jdbcSingleRoutingEncryptSelectResultPath, skipBegin, skipEnd);
        if (jdbcSingleRoutingEncryptSelectResult.size() > 0) {
            String jdbcSelectSql = (String)sqlConfig.get("jdbc.benchmark.singlerouting.encrypt.select.sql");
            BenchmarkResultBean jdbcSingleRoutingEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcSingleRoutingEncryptSelectResult, jdbcSelectSql, "Encrypt", "MYSQL", "SingleRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(jdbcSingleRoutingEncryptSelectResultBean);
        }
    }
    
    /**
     * Calculate result for single routing master-slave scenario.
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
    public void calculateSingleRoutingMasterslave(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        String proxySingleRoutingMasterSlaveInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.singlerouting.masterslave.insertupdatedelete.result");
        Map proxySingleRoutingMasterSlaveInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(proxySingleRoutingMasterSlaveInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (proxySingleRoutingMasterSlaveInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.singlerouting.masterslave.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.masterslave.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.masterslave.update.sql") +  ";\r\n";
            BenchmarkResultBean proxySingleRoutingMasterSlaveInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion,proxySingleRoutingMasterSlaveInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "MasterSlave", "ShardingProxy", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(proxySingleRoutingMasterSlaveInsertupdatedeleteResultBean);
        }
        String shardingjdbcSingleRoutingMasterSlaveInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.singlerouting.masterslave.insertupdatedelete.result");
        Map shardingjdbcSingleRoutingMasterSlaveInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcSingleRoutingMasterSlaveInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (shardingjdbcSingleRoutingMasterSlaveInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.singlerouting.masterslave.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.masterslave.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.masterslave.update.sql") +  ";\r\n";
            BenchmarkResultBean shardingjdbcSingleRoutingMasterSlaveInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcSingleRoutingMasterSlaveInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "MasterSlave", "ShardingJDBC", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(shardingjdbcSingleRoutingMasterSlaveInsertupdatedeleteResultBean);
        }
        String jdbcSingleRoutingMasterSlaveInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.singlerouting.masterslave.insertupdatedelete.result");
        Map jdbcSingleRoutingMasterSlaveInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(jdbcSingleRoutingMasterSlaveInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (jdbcSingleRoutingMasterSlaveInsertupdatedeleteResult.size() > 0) {
            String jdbcInsertUpdateDeleteSQL = (String)sqlConfig.get("jdbc.benchmark.singlerouting.masterslave.delete.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.singlerouting.masterslave.insert.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.singlerouting.masterslave.update.sql") +  ";\r\n";
            BenchmarkResultBean jdbcSingleRoutingMasterSlaveInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcSingleRoutingMasterSlaveInsertupdatedeleteResult, jdbcInsertUpdateDeleteSQL, "MasterSlave", "MYSQL", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(jdbcSingleRoutingMasterSlaveInsertupdatedeleteResultBean);
        }
        String proxySingleRoutingMasterSlaveSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.singlerouting.masterslave.select.result");
        Map proxySingleRoutingMasterSlaveSelectResult = BenchmarkResultParser.benchmarkStatistic(proxySingleRoutingMasterSlaveSelectResultPath, skipBegin, skipEnd);
        if (proxySingleRoutingMasterSlaveSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.singlerouting.masterslave.select.sql");
            BenchmarkResultBean proxySingleRoutingMasterSlaveSelectResultBean = new BenchmarkResultBean(benchmarkVersion, proxySingleRoutingMasterSlaveSelectResult, ssSelectSql, "MasterSlave", "ShardingProxy", "SingleRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(proxySingleRoutingMasterSlaveSelectResultBean);
        }
        String shardingjdbcSingleRoutingMasterSlaveSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.singlerouting.masterslave.select.result");
        Map shardingjdbcSingleRoutingMasterSlaveSelectResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcSingleRoutingMasterSlaveSelectResultPath, skipBegin, skipEnd);
        if (shardingjdbcSingleRoutingMasterSlaveSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.singlerouting.masterslave.select.sql");
            BenchmarkResultBean shardingjdbcSingleRoutingMasterSlaveSelectResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcSingleRoutingMasterSlaveSelectResult, ssSelectSql, "MasterSlave", "ShardingJDBC", "SingleRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(shardingjdbcSingleRoutingMasterSlaveSelectResultBean);
        }
        String jdbcSingleRoutingMasterSlaveSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.singlerouting.masterslave.select.result");
        Map jdbcSingleRoutingMasterSlaveSelectResult = BenchmarkResultParser.benchmarkStatistic(jdbcSingleRoutingMasterSlaveSelectResultPath, skipBegin, skipEnd);
        if (jdbcSingleRoutingMasterSlaveSelectResult.size() > 0) {
            String jdbcSelectSql = (String)sqlConfig.get("jdbc.benchmark.singlerouting.masterslave.select.sql");
            BenchmarkResultBean jdbcSingleRoutingMasterSlaveSelectResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcSingleRoutingMasterSlaveSelectResult, jdbcSelectSql, "MasterSlave", "MYSQL", "SingleRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(jdbcSingleRoutingMasterSlaveSelectResultBean);
        }
    }
    
    /**
     * Calculate result for single routing sharding scenario.
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
    public void calculateSingleRoutingSharding(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        String proxySingleRoutingShardingInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.singlerouting.sharding.insertupdatedelete.result");
        Map proxySingleRoutingShardingInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(proxySingleRoutingShardingInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (proxySingleRoutingShardingInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.singlerouting.sharding.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.sharding.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.sharding.update.sql") +  ";\r\n";
            BenchmarkResultBean proxySingleRoutingShardingInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion,proxySingleRoutingShardingInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Sharding", "ShardingProxy", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(proxySingleRoutingShardingInsertupdatedeleteResultBean);
        }
        String shardingjdbcSingleRoutingShardingInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.singlerouting.sharding.insertupdatedelete.result");
        Map shardingjdbcSingleRoutingShardingInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcSingleRoutingShardingInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (shardingjdbcSingleRoutingShardingInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.singlerouting.sharding.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.sharding.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.sharding.update.sql") +  ";\r\n";
            BenchmarkResultBean shardingjdbcSingleRoutingShardingInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcSingleRoutingShardingInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Sharding", "ShardingJDBC", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(shardingjdbcSingleRoutingShardingInsertupdatedeleteResultBean);
        }
        String jdbcSingleRoutingShardingInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.singlerouting.sharding.insertupdatedelete.result");
        Map jdbcSingleRoutingShardingInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(jdbcSingleRoutingShardingInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (jdbcSingleRoutingShardingInsertupdatedeleteResult.size() > 0) {
            String jdbcInsertUpdateDeleteSQL = (String)sqlConfig.get("jdbc.benchmark.singlerouting.sharding.delete.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.singlerouting.sharding.insert.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.singlerouting.sharding.update.sql") +  ";\r\n";
            BenchmarkResultBean jdbcSingleRoutingShardingInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcSingleRoutingShardingInsertupdatedeleteResult, jdbcInsertUpdateDeleteSQL, "Sharding", "MYSQL", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(jdbcSingleRoutingShardingInsertupdatedeleteResultBean);
        }
        String proxySingleRoutingShardingSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.singlerouting.sharding.select.result");
        Map proxySingleRoutingShardingSelectResult = BenchmarkResultParser.benchmarkStatistic(proxySingleRoutingShardingSelectResultPath, skipBegin, skipEnd);
        if (proxySingleRoutingShardingSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.singlerouting.sharding.select.sql");
            BenchmarkResultBean proxySingleRoutingShardingSelectResultBean = new BenchmarkResultBean(benchmarkVersion, proxySingleRoutingShardingSelectResult, ssSelectSql, "Sharding", "ShardingProxy", "SingleRouting", "Select", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(proxySingleRoutingShardingSelectResultBean);
        }
        String shardingjdbcSingleRoutingShardingSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.singlerouting.sharding.select.result");
        Map shardingjdbcSingleRoutingShardingSelectResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcSingleRoutingShardingSelectResultPath, skipBegin, skipEnd);
        if (shardingjdbcSingleRoutingShardingSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.singlerouting.sharding.select.sql");
            BenchmarkResultBean shardingjdbcSingleRoutingShardingSelectResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcSingleRoutingShardingSelectResult, ssSelectSql, "Sharding", "ShardingJDBC", "SingleRouting", "Select", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(shardingjdbcSingleRoutingShardingSelectResultBean);
        }
        String jdbcSingleRoutingShardingSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.singlerouting.sharding.select.result");
        Map jdbcSingleRoutingShardingSelectResult = BenchmarkResultParser.benchmarkStatistic(jdbcSingleRoutingShardingSelectResultPath, skipBegin, skipEnd);
        if (jdbcSingleRoutingShardingSelectResult.size() > 0) {
            String jdbcSelectSql = (String)sqlConfig.get("jdbc.benchmark.singlerouting.sharding.select.sql");
            BenchmarkResultBean jdbcSingleRoutingShardingSelectResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcSingleRoutingShardingSelectResult, jdbcSelectSql, "Sharding", "MYSQL", "SingleRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(jdbcSingleRoutingShardingSelectResultBean);
        }
    }
    
    /**
     * Calculate result for single routing sharding-master-slave-encrypt scenario.
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
    public void calculateSingleRoutingShardingMasterslaveEncrypt(Map benchmarkResultPath, Map sqlConfig, String benchmarkVersion, int skipBegin, int skipEnd, int concurrency, long updateTime, int dbShardingCount, int tableShardingCount) {
        String proxySingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.singlerouting.shardingmasterslaveencrypt.insertupdatedelete.result");
        Map proxySingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(proxySingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (proxySingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.singlerouting.shardingmasterslaveencrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.shardingmasterslaveencrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.shardingmasterslaveencrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean proxySingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion,proxySingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Sharding+Master+Slave+Encrypt", "ShardingProxy", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(proxySingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean);
        }
        String shardingjdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.singlerouting.shardingmasterslaveencrypt.insertupdatedelete.result");
        Map shardingjdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (shardingjdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult.size() > 0) {
            String ssInsertUpdateDeleteSQL = (String)sqlConfig.get("ss.benchmark.singlerouting.shardingmasterslaveencrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.shardingmasterslaveencrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("ss.benchmark.singlerouting.shardingmasterslaveencrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean shardingjdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult, ssInsertUpdateDeleteSQL, "Sharding+Master+Slave+Encrypt", "ShardingJDBC", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(shardingjdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean);
        }
        String jdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.singlerouting.shardingmasterslaveencrypt.insertupdatedelete.result");
        Map jdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult = BenchmarkResultParser.benchmarkStatistic(jdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultPath, skipBegin, skipEnd);
        if (jdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult.size() > 0) {
            String jdbcInsertUpdateDeleteSQL = (String)sqlConfig.get("jdbc.benchmark.singlerouting.shardingmasterslaveencrypt.delete.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.singlerouting.shardingmasterslaveencrypt.insert.sql") + ";\r\n" + (String)sqlConfig.get("jdbc.benchmark.singlerouting.shardingmasterslaveencrypt.update.sql") +  ";\r\n";
            BenchmarkResultBean jdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResult, jdbcInsertUpdateDeleteSQL, "Sharding+Master+Slave+Encrypt", "MYSQL", "SingleRouting", "Insert+Update+delete", concurrency, updateTime, 0, 0);
            result.add(jdbcSingleRoutingShardingMasterSlaveEncryptInsertupdatedeleteResultBean);
        }
        String proxySingleRoutingShardingMasterSlaveEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.proxy.singlerouting.shardingmasterslaveencrypt.select.result");
        Map proxySingleRoutingShardingMasterSlaveEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(proxySingleRoutingShardingMasterSlaveEncryptSelectResultPath, skipBegin, skipEnd);
        if (proxySingleRoutingShardingMasterSlaveEncryptSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.singlerouting.shardingmasterslaveencrypt.select.sql");
            BenchmarkResultBean proxySingleRoutingShardingMasterSlaveEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, proxySingleRoutingShardingMasterSlaveEncryptSelectResult, ssSelectSql, "Sharding+Master+Slave+Encrypt", "ShardingProxy", "SingleRouting", "Select", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(proxySingleRoutingShardingMasterSlaveEncryptSelectResultBean);
        }
        String shardingjdbcSingleRoutingShardingMasterSlaveEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.shardingjdbc.singlerouting.shardingmasterslaveencrypt.select.result");
        Map shardingjdbcSingleRoutingShardingMasterSlaveEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(shardingjdbcSingleRoutingShardingMasterSlaveEncryptSelectResultPath, skipBegin, skipEnd);
        if (shardingjdbcSingleRoutingShardingMasterSlaveEncryptSelectResult.size() > 0) {
            String ssSelectSql = (String)sqlConfig.get("ss.benchmark.singlerouting.shardingmasterslaveencrypt.select.sql");
            BenchmarkResultBean shardingjdbcSingleRoutingShardingMasterSlaveEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, shardingjdbcSingleRoutingShardingMasterSlaveEncryptSelectResult, ssSelectSql, "Sharding+Master+Slave+Encrypt", "ShardingJDBC", "SingleRouting", "Select", concurrency, updateTime, dbShardingCount, tableShardingCount);
            result.add(shardingjdbcSingleRoutingShardingMasterSlaveEncryptSelectResultBean);
        }
        String jdbcSingleRoutingShardingMasterSlaveEncryptSelectResultPath = (String) benchmarkResultPath.get("ss.benchmark.jdbc.singlerouting.shardingmasterslaveencrypt.select.result");
        Map jdbcSingleRoutingShardingMasterSlaveEncryptSelectResult = BenchmarkResultParser.benchmarkStatistic(jdbcSingleRoutingShardingMasterSlaveEncryptSelectResultPath, skipBegin, skipEnd);
        if (jdbcSingleRoutingShardingMasterSlaveEncryptSelectResult.size() > 0) {
            String jdbcSelectSql = (String)sqlConfig.get("jdbc.benchmark.singlerouting.shardingmasterslaveencrypt.select.sql");
            BenchmarkResultBean jdbcSingleRoutingShardingMasterSlaveEncryptSelectResultBean = new BenchmarkResultBean(benchmarkVersion, jdbcSingleRoutingShardingMasterSlaveEncryptSelectResult, jdbcSelectSql, "Sharding+Master+Slave+Encrypt", "MYSQL", "SingleRouting", "Select", concurrency, updateTime, 0, 0);
            result.add(jdbcSingleRoutingShardingMasterSlaveEncryptSelectResultBean);
        }
    }
    
    
    /**
     * Calculate average result for single routing scenario.
     *
     * @param datasource
     * @param sqlConfig
     * @param noShardingParams
     * @param shardingParams
     * @return
     */
    public  List<BenchmarkResultBean> calculateSingleroutingScenarioAvgResult(DataSource datasource, Map sqlConfig, List noShardingParams, List shardingParams){
        List<BenchmarkResultBean> singleRoutingCalResult = new ArrayList<BenchmarkResultBean>(10);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.encrypt.shardingjdbc.select.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.encrypt.proxy.select.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.encrypt.mysql.select.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.encrypt.shardingjdbc.insertupdatedelete.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.encrypt.proxy.insertupdatedelete.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.encrypt.mysql.insertupdatedelete.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.masterslave.shardingjdbc.select.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.masterslave.proxy.select.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.masterslave.mysql.select.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.masterslave.shardingjdbc.insertupdatedelete.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.masterslave.proxy.insertupdatedelete.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.masterslave.mysql.insertupdatedelete.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.sharding.shardingjdbc.select.sql"), shardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.sharding.proxy.select.sql"), shardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.sharding.mysql.select.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.sharding.shardingjdbc.insertupdatedelete.sql"), shardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.sharding.proxy.insertupdatedelete.sql"), shardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.sharding.mysql.insertupdatedelete.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.shardingmasterslaveencrypt.shardingjdbc.select.sql"), shardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.shardingmasterslaveencrypt.proxy.select.sql"), shardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.shardingmasterslaveencrypt.mysql.select.sql"), noShardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.shardingmasterslaveencrypt.shardingjdbc.insertupdatedelete.sql"), shardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.shardingmasterslaveencrypt.proxy.insertupdatedelete.sql"), shardingParams, singleRoutingCalResult);
        getTargetResult(datasource, (String)sqlConfig.get("ss.benchmark.result.singlerouting.shardingmasterslaveencrypt.mysql.insertupdatedelete.sql"), noShardingParams, singleRoutingCalResult);
    
        return singleRoutingCalResult;
    }
}

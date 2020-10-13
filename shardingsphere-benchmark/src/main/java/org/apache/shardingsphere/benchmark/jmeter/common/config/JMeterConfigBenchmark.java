package org.apache.shardingsphere.benchmark.jmeter.common.config;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.common.file.jmeter.BenchmarkResultDirManagement;
import org.apache.shardingsphere.benchmark.common.file.properties.BenchmarkConfigProperties;
import org.apache.shardingsphere.benchmark.common.file.xml.BenchmarkConfigJmx;
import org.apache.shardingsphere.benchmark.common.file.yaml.BenchmarkConfigYaml;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

public class JMeterConfigBenchmark extends JMeterBenchmarkBase {
    
    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterConfigBenchmark");
        results.sampleStart();
        String benchmarkBasePath = (String)userConfig.get("shardingsphere.benchmark.project.base.path");
        String benchmarkOutputBasePath = (String)userConfig.get("shardingsphere.benchmark.result.base.path");
        int shardingDbCount = Integer.valueOf((String)userConfig.get("shardingsphere.sharding.db.count")).intValue();
        int shardingTableCount = Integer.valueOf((String)userConfig.get("shardingsphere.sharding.table.count")).intValue();
        int maxConnectionCount = Integer.valueOf((String)userConfig.get("shardingsphere.maximum.connection.count")).intValue();
        int minConnectionCount = Integer.valueOf((String)userConfig.get("shardingsphere.minimum.connection.count")).intValue();
        int maxConnectionPerQueryCount = Integer.valueOf((String)userConfig.get("shardingsphere.maximum.connection.count.for.each.query")).intValue();
        int jmeterLoopCount = Integer.valueOf((String)userConfig.get("shardingsphere.jmeter.loop.count")).intValue();
        int jmeterConcurrencyCount = Integer.valueOf((String)userConfig.get("shardingsphere.jmeter.concurrency.count")).intValue();
        String createdDatabaseName = (String)userConfig.get("shardingsphere.benchmark.database.name");
        String createTableName = (String)userConfig.get("shardingsphere.sharding.benchmark.table_name");
        String databaseMachineList = (String)userConfig.get("shardingsphere.benchmark.database.machine.host.list");
        // Config jmx files
        BenchmarkConfigJmx.modifyBenchmarkOutputBasePath(benchmarkBasePath, benchmarkOutputBasePath, jmeterConcurrencyCount, jmeterLoopCount);
        // Config property files
        BenchmarkConfigProperties.modifyBenchmarkOutputConfig(benchmarkBasePath, benchmarkOutputBasePath);
        // Config yaml files
        BenchmarkConfigYaml.modifyBenchmarkYamlFile(benchmarkBasePath, shardingDbCount, shardingTableCount, maxConnectionCount, minConnectionCount, maxConnectionPerQueryCount);
        // Manage benchmark result dirs
        BenchmarkResultDirManagement.manageResultDir(userConfig);
        // Create databases/tables
        String[] databaseMachineArrays = databaseMachineList.split(";");
        try {
            for(int i = 0; i < databaseMachineArrays.length; i++){
                String dbHost = databaseMachineArrays[i];
                initBenchmarkDB(dbHost, createdDatabaseName, createTableName, shardingTableCount);
            }
            initBenchmarkResultDB((String)userConfig.get("shardingsphere.benchmark.result.database.host"), (String)userConfig.get("shardingsphere.benchmark.result.datasource"));
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        } finally {
            results.sampleEnd();
        }
        
        results.sampleEnd();
        return results;
    }
    
    /**
     * Create datasource and table for benchmark result.
     *
     * @param host
     * @param createdResultDatabaseName
     */
    public void initBenchmarkResultDB(String host, String createdResultDatabaseName){
        String createdDatabaseSql = initDbSqlList.get(2);
        String createdResultTableSql = initDbSqlList.get(3);
        String createdAvgResultTableSql = initDbSqlList.get(4);
        DataSource dataSource1 = JDBCDataSourceUtil.initDb("information_schema", host, Integer.valueOf((String)userConfig.get("shardingsphere.benchmark.database.port")).intValue(), (String) userConfig.get("shardingsphere.benchmark.database.username"), (String) userConfig.get("shardingsphere.benchmark.database.password"));
        Connection connection1 = null;
        try {
            connection1 = dataSource1.getConnection();
            Statement stat1 = connection1.createStatement();
            stat1.executeUpdate(createdDatabaseSql);
            stat1.close();
            connection1.close();
            DataSource createdDataSource1 = JDBCDataSourceUtil.initDb(createdResultDatabaseName, host, Integer.valueOf((String)userConfig.get("shardingsphere.benchmark.database.port")).intValue(), (String) userConfig.get("shardingsphere.benchmark.database.username"), (String) userConfig.get("shardingsphere.benchmark.database.password"));
            Connection createdConnection1 = createdDataSource1.getConnection();
            Statement createdStat1 = createdConnection1.createStatement();
            createdStat1.executeUpdate(createdAvgResultTableSql);
            createdStat1.close();
            createdConnection1.close();
            createdDataSource1 = JDBCDataSourceUtil.initDb(createdResultDatabaseName, host, Integer.valueOf((String)userConfig.get("shardingsphere.benchmark.database.port")).intValue(), (String) userConfig.get("shardingsphere.benchmark.database.username"), (String) userConfig.get("shardingsphere.benchmark.database.password"));
            createdConnection1 = createdDataSource1.getConnection();
            createdStat1 = createdConnection1.createStatement();
            createdStat1.executeUpdate(createdResultTableSql);
            createdStat1.close();
            createdConnection1.close();
        } catch (SQLException ex) {
            ex.printStackTrace();
        } finally {}
    }
    
    /**
     * Create benchmark datasource and tables.
     *
     * @param host
     * @param createdDatabaseName
     * @param createdTableName
     * @param tableCount
     * @throws SQLException
     */
    public void initBenchmarkDB(String host, String createdDatabaseName, String createdTableName, int tableCount) throws SQLException {
        String createdDatabaseSql = initDbSqlList.get(0);
        createdDatabaseSql = createdDatabaseSql.replace("default_database", createdDatabaseName);
        String createdTableSql = initDbSqlList.get(1);
        DataSource dataSource1 = JDBCDataSourceUtil.initDb("information_schema", host, Integer.valueOf((String)userConfig.get("shardingsphere.benchmark.database.port")).intValue(), (String) userConfig.get("shardingsphere.benchmark.database.username"), (String) dbConfig.get("shardingsphere.benchmark.database.password"));
        Connection connection1 = dataSource1.getConnection();
        Statement stat1 = connection1.createStatement();
        stat1.executeUpdate(createdDatabaseSql);
        stat1.close();
        connection1.close();
        DataSource createdDataSource1 = JDBCDataSourceUtil.initDb(createdDatabaseName, host, Integer.valueOf((String)userConfig.get("shardingsphere.benchmark.database.port")).intValue(), (String) userConfig.get("shardingsphere.benchmark.database.username"), (String) dbConfig.get("shardingsphere.benchmark.database.password"));
        Connection createdConnection1 = createdDataSource1.getConnection();
        Statement createdStat1 = createdConnection1.createStatement();
        String createdNoShardingSql = createdTableSql.replace("default_table", createdTableName);
        createdStat1.executeUpdate(createdNoShardingSql);
        createdStat1.close();
        createdConnection1.close();
        String shardingTableSql = "";
        for (int i = 0; i < tableCount; i++) {
            Connection createdConnection2 = createdDataSource1.getConnection();
            Statement createdStat2 = createdConnection2.createStatement();
            shardingTableSql = createdTableSql.replace("default_table", createdTableName + i);
            System.out.println(shardingTableSql);
            createdStat2.executeUpdate(shardingTableSql);
            createdStat2.close();
            createdConnection2.close();
        }
    }
}

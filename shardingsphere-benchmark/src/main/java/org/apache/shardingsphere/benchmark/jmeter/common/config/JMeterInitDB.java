
package org.apache.shardingsphere.benchmark.jmeter.common.config;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

public class JMeterInitDB extends JMeterBenchmarkBase {

    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterInitDB");
        results.sampleStart();

        int tableCount = Integer.valueOf((String)userConfig.get("shardingsphere.sharding.table.count")).intValue();
        String createdDatabaseName = (String)userConfig.get("shardingsphere.benchmark.database.name");
        String createTableName = (String)userConfig.get("shardingsphere.sharding.benchmark.table_name");
        
        String databaseMachineList = (String)userConfig.get("shardingsphere.benchmark.database.machine.host.list");
        String[] databaseMachineArrays = databaseMachineList.split(";");

        try {
            for(int i = 0; i < databaseMachineArrays.length; i++){
                String dbHost = databaseMachineArrays[i];
                initDb(dbHost, createdDatabaseName, createTableName, tableCount);
            }
            initBenchmarkResultDb((String)userConfig.get("shardingsphere.benchmark.result.database.host"), (String)userConfig.get("shardingsphere.benchmark.result.datasource"));
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        } finally {
            results.sampleEnd();
        }
        return results;
    }
    
    
    public void initBenchmarkResultDb(String host, String createdResultDatabaseName){
        String createdDatabaseSql = initDbSqlList.get(2);
        String createdResultTableSql = initDbSqlList.get(3);
        String createdAvgResultTableSql = initDbSqlList.get(3);
    
        DataSource dataSource1 = JDBCDataSourceUtil.initDb("information_schema", host, (int) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.port"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.username"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.password"));
        Connection connection1 = null;
        try {
            connection1 = dataSource1.getConnection();
            Statement stat1 = connection1.createStatement();
            stat1.executeUpdate(createdDatabaseSql);
            stat1.close();
            connection1.close();
    
            DataSource createdDataSource1 = JDBCDataSourceUtil.initDb(createdResultDatabaseName, host, (int) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.port"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.username"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.password"));
            Connection createdConnection1 = createdDataSource1.getConnection();
            Statement createdStat1 = createdConnection1.createStatement();
            createdStat1.executeUpdate(createdAvgResultTableSql);
            createdStat1.close();
            createdConnection1.close();
    
            createdDataSource1 = JDBCDataSourceUtil.initDb(createdResultDatabaseName, host, (int) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.port"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.username"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.password"));
            createdConnection1 = createdDataSource1.getConnection();
            createdStat1 = createdConnection1.createStatement();
            createdStat1.executeUpdate(createdResultTableSql);
            createdStat1.close();
            createdConnection1.close();
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        } finally {
        }
    }
    
    public void initDb(String host, String createdDatabaseName, String createdTableName, int tableCount) throws SQLException {
        
        String createdDatabaseSql = initDbSqlList.get(0);
        createdDatabaseSql = createdDatabaseSql.replace("default_database", createdDatabaseName);
        String createdTableSql = initDbSqlList.get(1);
        
        DataSource dataSource1 = JDBCDataSourceUtil.initDb("information_schema", host, (int) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.port"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.username"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.password"));
        Connection connection1 = dataSource1.getConnection();
        Statement stat1 = connection1.createStatement();
        stat1.executeUpdate(createdDatabaseSql);
        stat1.close();
        connection1.close();
    
        DataSource createdDataSource1 = JDBCDataSourceUtil.initDb(createdDatabaseName, host, (int) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.port"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.username"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.password"));
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


package org.apache.shardingsphere.benchmark.jmeter.common.datapreparation.jdbc;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Random;

public class JMeterJDBCCommonInsert extends JMeterBenchmarkBase {
    
    public static DataSource dataSource;
    public Random r = new Random(1);
    public int tableCount = Integer.valueOf((String)userConfig.get("shardingsphere.sharding.table.count")).intValue();
    static {
        dataSource = JDBCDataSourceUtil.initDb((String) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.datasource"),
                (String) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.host"), (int) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.port"),
                (String) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.username"), (String) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.password"));
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterJDBCCommonShardingMasterSlaveEncryptInsert");
        results.sampleStart();
        Connection connection = null;

        try {
            connection = dataSource.getConnection();
            String insertSql = (String) sqlConfig.get("common.jdbc.insert.sql");
            List insertParams = convertParams((List) sqlConfig.get("common.jdbc.insert.values"), r.nextInt(tableCount));
            JDBCDataSourceUtil.insert(connection, insertSql, insertParams);
        } catch (SQLException e) {
            results.setSuccessful(false);
            e.printStackTrace();
        } catch (Exception e) {
            results.setSuccessful(false);
            e.printStackTrace();
        } finally {
            try {
                connection.close();
            } catch (SQLException throwables) {
                throwables.printStackTrace();
            }
            results.sampleEnd();
        }
        return results;
    }
}

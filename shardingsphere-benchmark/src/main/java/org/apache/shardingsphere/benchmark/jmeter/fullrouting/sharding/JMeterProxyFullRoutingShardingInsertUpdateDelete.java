package org.apache.shardingsphere.benchmark.jmeter.fullrouting.sharding;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

public class JMeterProxyFullRoutingShardingInsertUpdateDelete extends JMeterBenchmarkBase {

    public static DataSource dataSource;

    static {
        dataSource = JDBCDataSourceUtil.initDb((String) dbConfig.get("ss.proxy.db.datasource"),
                (String) dbConfig.get("ss.proxy.host"), (int) dbConfig.get("ss.proxy.port"),
                (String) dbConfig.get("ss.proxy.db.username"), (String) dbConfig.get("ss.proxy.db.password"));
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterProxyFullRoutingEncryptInsert");
        results.sampleStart();
        Connection connection = null;

        try {
            connection = dataSource.getConnection();
            String updateSql = (String) sqlConfig.get("ss.benchmark.fullrouting.sharding.update.sql");
            List updateParams = convertParams((List) sqlConfig.get("ss.benchmark.fullrouting.sharding.update.values"));
            JDBCDataSourceUtil.update(connection, updateSql, updateParams);

            /*String deleteSql = (String)sqlConfig.get("ss.benchmark.fullrouting.sharding.delete.sql");
            List deleteParams = convertParams((List)sqlConfig.get("ss.benchmark.fullrouting.sharding.delete.values"));
            JDBCDataSourceUtil.delete(connection, deleteSql, deleteParams);*/
            results.setSuccessful(true);
        } catch (SQLException e) {
            results.setSuccessful(false);
            e.printStackTrace();
        } catch (Exception e) {
            results.setSuccessful(false);
            e.printStackTrace();
        } finally {
            results.sampleEnd();
            try {
                connection.close();
            } catch (SQLException throwables) {
                throwables.printStackTrace();
            }
        }
        return results;
    }
}

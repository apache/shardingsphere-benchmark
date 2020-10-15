package org.apache.shardingsphere.benchmark.jmeter.fullrouting.encrypt;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

/**
 * Refactor old case test_plan/jdbc_sharding_new/select_one.jmx
 * <p>
 * Sharding scenario
 * <p>
 * Its actualDataNodes: ds{0..2}.sbtest${0..99}
 * Its dataSource.algorithm expression: ds_${id % 3}
 * Its table.algorithm expression: sbtest${k % 100}
 * Its type: INLINE
 **/

public class JMeterJDBCFullRoutingEncryptSelect extends JMeterBenchmarkBase {

    public static DataSource dataSource;

    static {
        dataSource = JDBCDataSourceUtil.initDb((String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.datasource"),
                (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.host"), (int) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.port"),
                (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.username"), (String) dbConfig.get("jdbc.benchmark.fullrouting.encrypt.ds0.password"));
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterJDBCFullRoutingEncryptSelect");
        results.sampleStart();
        Connection connection = null;

        try {
            connection = dataSource.getConnection();
            String selectSql = (String) sqlConfig.get("jdbc.benchmark.fullrouting.encrypt.select.sql");
            //List selectParams = convertParams((List) sqlConfig.get("jdbc.benchmark.fullrouting.encrypt.select.values"));
            JDBCDataSourceUtil.select(connection, selectSql, null);
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
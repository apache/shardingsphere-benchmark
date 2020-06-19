package org.apache.shardingsphere.benchmark.jmeter.jdbc;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

/**
 * Refactor old case test_plan/new_sharding/jdbc_clean.jmx
 */
public class JMeterJDBCDeleteSbTest6 extends AbstractJavaSamplerClient {

    public static final int DB_PORT = 3306;

    public static final String DB_PASSWORD = "";

    public static final String DB_USER_NAME = "root";

    public static final String DB_HOST = "10.222.16.97";

    public static final String DB_NAME = "baitiao_test";

    public static final String TABLE_NAME = "sbtest6";

    public static final String DELETE_SQL = "truncate table " + TABLE_NAME;


    // Init mysql database.
    static {
        JDBCDataSourceUtil.initDb(DB_NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("JdbcSingleTableDelete");
        results.sampleStart();

        try {

            conn = JDBCDataSourceUtil.getDataSource(DB_NAME).getConnection();
            JDBCDataSourceUtil.delete(conn, DELETE_SQL, null);
            results.setSuccessful(true);

        } catch (SQLException e) {

            results.setSuccessful(false);
            e.printStackTrace();

        } finally {
            results.sampleEnd();
            try {
                JDBCDataSourceUtil.close(conn);
            } catch (SQLException throwables) {
                throwables.printStackTrace();
            }

        }

        return results;
    }

    @Override
    public Arguments getDefaultParameters() {

        return null;
    }


    @Override
    public void setupTest(JavaSamplerContext context) {}
}

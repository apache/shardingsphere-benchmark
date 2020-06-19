package org.apache.shardingsphere.benchmark.jmeter.proxy;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;

import java.sql.Connection;
import java.sql.SQLException;


/**
 * Refactor old case test_plan/master_slave_new/delete_all.jmx
 */
public class JMeterProxyMasterSlaveDeleteSbTest extends AbstractJavaSamplerClient {

    public static final int DB_PORT = 3307;

    public static final String DB_PASSWORD = "root";

    public static final String DB_USER_NAME = "root";

    public static final String DB_HOST = "10.222.16.178";

    public static final String DB_NAME = "sharding_db";

    public static final String TABLE_NAME = "sbtest3";

    public static final String DELETE_SQL = "truncate table " + TABLE_NAME;


    static {
        JDBCDataSourceUtil.initDb(DB_NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("SPPerformanceSingleTableClean");
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

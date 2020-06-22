package org.apache.shardingsphere.benchmark.jmeter.proxy;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

/**
 * Refactor old case test_plan/new_sharding/sp_select.jmx
 * Proxy scenario:
 *  Its actualDataNodes: press_test_${0..3}.sbtest4
 *  Its dataSource.algorithm expression: press_test_${id % 4}
 *  Its table.algorithm expression: sbtest4
 *  Its type: INLINE
 *  Its config: yaml/proxy/config-sharding_alldb_singletable_new.yaml
 */
public class JMeterProxyShardingAllDbSingleTableSbTest4Select extends AbstractJavaSamplerClient {

    public Connection connection;

    public static DataSource dataSource;

    public static final int DB_PORT = 3307;

    public static final String DB_PASSWORD = "root";

    public static final String DB_USER_NAME = "root";

    public static final String DB_HOST = "10.222.16.178";

    public static final String DB_NAME = "sharding_db";

    public static final String TABLE_NAME = "sbtest";

    public static final String SELECT_SQL = "select max(id) from " + TABLE_NAME + " where id % 4 = 1";

    // Init dataSource
    static {
        dataSource = JDBCDataSourceUtil.initDb(DB_NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        SampleResult results = new SampleResult();
        results.setSampleLabel("SPPerformanceSingleTableClean");
        results.sampleStart();

        try {

            JDBCDataSourceUtil.delete(connection, SELECT_SQL, null);
            results.setSuccessful(true);

        } catch (SQLException e) {
            results.setSuccessful(false);
            e.printStackTrace();
        } finally {
            results.sampleEnd();
        }

        return results;
    }

    @Override
    public Arguments getDefaultParameters() {
        return null;
    }

    /**
     * Get database connection.
     *
     * @param context
     */
    @Override
    public void setupTest(JavaSamplerContext context) {

        try {
            connection = dataSource.getConnection();
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
    }

    /**
     * Close dataSource connection.
     *
     * @param context
     */
    @Override
    public void teardownTest(JavaSamplerContext context) {

        try {
            JDBCDataSourceUtil.close(connection);
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
    }
}

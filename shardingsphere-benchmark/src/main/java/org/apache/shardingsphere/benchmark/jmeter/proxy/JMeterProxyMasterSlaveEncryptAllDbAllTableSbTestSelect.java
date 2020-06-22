package org.apache.shardingsphere.benchmark.jmeter.proxy;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import service.api.entity.Iou;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Refactor old case test_plan/ms_enc_sharding_new/select_one.jmx
 *
 * Proxy scenario:
 *  Its actualDataNodes: ms_ds_${0..3}.sbtest${0..1023}
 *  Its database algorithm.expression: ms_ds_${id % 4}
 *  Its table algorithm.expression: sbtest${k % 1024}
 *  Its master-slave: ms_ds_* -> slave_ds_*
 *  Its type: INLINE
 *  Its config: yaml/proxy/config-master_slave_enc_sharding.yaml
 *
 */
public class JMeterProxyMasterSlaveEncryptAllDbAllTableSbTestSelect extends AbstractJavaSamplerClient {

    public Connection connection;

    public static DataSource dataSource;

    public static final int DB_PORT = 3307;

    public static final String DB_PASSWORD = "root";

    public static final String DB_USER_NAME = "root";

    public static final String DB_HOST = "10.222.16.178";

    public static final String TABLE_NAME = "sbtest";

    public static final String DB_NAME = "sharding_db";

    public static final String SELECT_SQL = "SELECT id,k from " + TABLE_NAME + " ignore index(`PRIMARY`) where id=1 and k=3";

    // Init dataSource.
    static {
        dataSource = JDBCDataSourceUtil.initDb(DB_NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
    }

    /**
     * Execute select action.
     *
     * @param context
     * @return
     */
    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        SampleResult results = new SampleResult();
        results.setSampleLabel("SPEncryptShardingSelect");
        results.sampleStart();

        try {

            JDBCDataSourceUtil.select(connection, SELECT_SQL, null);
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

    public List getIou(ResultSet resultSet) throws SQLException {
        List<Iou> result = new LinkedList<>();
        while (resultSet.next()) {
            Iou iou = new Iou();
            iou.setK(resultSet.getInt(2));
            result.add(iou);
        }
        return result;
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

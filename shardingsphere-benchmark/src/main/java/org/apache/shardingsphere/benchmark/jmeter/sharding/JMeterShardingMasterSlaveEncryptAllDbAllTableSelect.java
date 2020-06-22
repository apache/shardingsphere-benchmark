package org.apache.shardingsphere.benchmark.jmeter.sharding;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.db.shardingjdbc.ShardingConfigType;
import org.apache.shardingsphere.benchmark.db.shardingjdbc.ShardingJDBCDataSourceFactory;

import javax.sql.DataSource;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;


/**
 * Refactor old case test_plan/jdbc_ms_enc_sharding_new/select_one.jmx
 *
 * Sharding scenario:
 *  Its actualDataNodes: ms_ds_${0..3}.sbtest${0..1023}
 *  Its dataSource.algorithm expression: ms_ds_${id % 4}
 *  Its table.algorithm expression: sbtest${k % 1024}
 *  Its type: INLINE
 *  Its encrypt: aes, md5
 *  Its master-slave:  ms_ds_* -> ds_*_slave_*
 *
 */
public class JMeterShardingMasterSlaveEncryptAllDbAllTableSelect extends AbstractJavaSamplerClient {

    public Connection connection;

    public static DataSource dataSource;

    public static final String TABLE_NAME = "sbtest";

    public static final String SELECT_SQL = "SELECT id,k from " + TABLE_NAME + " where id=1 and k=1";

    // Init dataSource.
    static{
        try {
            dataSource = ShardingJDBCDataSourceFactory.newInstance (ShardingConfigType.MASTER_SLAVE_ENCRYPT_SHARDING_CONFIG);
        } catch (IOException e) {
            e.printStackTrace();
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
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
        results.setSampleLabel("SJPerformanceMSSelect");
        results.sampleStart();

        try {

            JDBCDataSourceUtil.select(connection, SELECT_SQL, null);
            results.setSuccessful(true);

        } catch (SQLException e) {

            results.setSuccessful(false);
            e.printStackTrace();

        } catch (Exception e){

            results.setSuccessful(false);
            e.printStackTrace();

        }finally {

            results.sampleEnd();
        }

        return results;
    }

    @Override
    public Arguments getDefaultParameters() {
        return null;
    }

    /**
     * Get dataSource connection.
     *
     * @param context
     */
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

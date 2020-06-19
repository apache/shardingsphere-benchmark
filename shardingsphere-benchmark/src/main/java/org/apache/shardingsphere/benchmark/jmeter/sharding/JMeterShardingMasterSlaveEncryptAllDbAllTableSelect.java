package org.apache.shardingsphere.benchmark.jmeter.sharding;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.db.shardingjdbc.ShardingConfigType;
import org.apache.shardingsphere.benchmark.db.shardingjdbc.ShardingJDBCDataSourceFactory;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;


/**
 * Refactor old case test_plan/jdbc_ms_enc_sharding_new/select_one.jmx
 */
public class JMeterShardingMasterSlaveEncryptAllDbAllTableSelect extends AbstractJavaSamplerClient {

    public static final String TABLE_NAME = "sbtest";

    public static final String SELECT_SQL = "SELECT id,k from " + TABLE_NAME + " where id=1 and k=1";


    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("SJPerformanceMSSelect");
        results.sampleStart();

        try {

            DataSource dataSource = ShardingJDBCDataSourceFactory.newInstance
                    (ShardingConfigType.MASTER_SLAVE_ENCRYPT_SHARDING_CONFIG);

            conn = dataSource.getConnection();
            JDBCDataSourceUtil.select(conn, SELECT_SQL, null);
            results.setSuccessful(true);

        } catch (SQLException e) {

            results.setSuccessful(false);
            e.printStackTrace();

        } catch (Exception e){

            results.setSuccessful(false);
            e.printStackTrace();

        }finally {

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
    public void setupTest(JavaSamplerContext context) {
    }
}

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
 * Refactor old case test_plan/new_sharding/sj_clean_new.jmx
 */
public class JMeterShardingAllDbSingleTableSbTest4Delete extends AbstractJavaSamplerClient {

    public static final String TABLE_NAME = "sbtest";

    public static final String DELETE_SQL = "truncate table " + TABLE_NAME;


    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("SJPerformanceMSInsert");
        results.sampleStart();

        try {

            DataSource dataSource = ShardingJDBCDataSourceFactory.newInstance
                    (ShardingConfigType.ALL_DATABASE_SINGLE_TABLE_SHARDING_NEW_CONFIG);

            conn = dataSource.getConnection();
            JDBCDataSourceUtil.delete(conn, DELETE_SQL, null);


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
    public void setupTest(JavaSamplerContext context) {}
}

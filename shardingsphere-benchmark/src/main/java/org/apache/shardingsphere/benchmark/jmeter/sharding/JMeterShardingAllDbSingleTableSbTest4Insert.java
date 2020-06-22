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
import java.util.Arrays;
import java.util.List;

/**
 * Refactor old case test_plan/new_sharding/sj_insert.jmx
 *
 * Sharding scenario:
 *  Its actualDataNodes: {0..3}.sbtest3
 *  Its dataSource.algorithm expression: ds_${id % 4}
 *  Its table.algorithm expression: sbtest3
 *  Its type: INLINE
 *
 */
public class JMeterShardingAllDbSingleTableSbTest4Insert extends AbstractJavaSamplerClient {

    public Connection connection;

    public static DataSource dataSource;

    public static final String TABLE_NAME = "sbtest";

    public static final String INSERT_SQL = "INSERT INTO " + TABLE_NAME + " (k,c,pad) VALUES (?,?,?)";

    // Init dataSource.
    static{
        try {
            dataSource = ShardingJDBCDataSourceFactory.newInstance (ShardingConfigType.ALL_DATABASE_SINGLE_TABLE_SHARDING_NEW_CONFIG);
        } catch (IOException e) {
            e.printStackTrace();
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        SampleResult results = new SampleResult();
        results.setSampleLabel("SJPerformancecleanclean");
        results.sampleStart();

        try {

            //insert an record
            List insertParams = Arrays.asList(1, "##-####", "##-####");
            JDBCDataSourceUtil.insert(connection, INSERT_SQL, insertParams);


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

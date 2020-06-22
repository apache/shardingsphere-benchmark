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
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

/**
 * Refactor old case test_plan/jdbc_ms_new/all_test.jmx.
 * Sharding scenario:
 *  Its master-slave:  ds_master* -> ds_slave*
 *
 */
public class JMeterShardingMasterSlaveInsertUpdateDelete extends AbstractJavaSamplerClient {

    public Connection connection;

    public static DataSource dataSource;

    public static final String TABLE_NAME = "sbtest3";

    public static final String DELETE_SQL = "delete from " + TABLE_NAME + " where k=? and id=?";

    public static final String INSERT_SQL = "insert into " + TABLE_NAME + " (k,c,pad) VALUES (?,?,?)";

    public static final String UPDATE_SQL = "update " + TABLE_NAME + " set c=?,pad =? where id=? and k=3";

    // Init dataSource.
    static{
        try {
            dataSource = ShardingJDBCDataSourceFactory.newInstance (ShardingConfigType.MASTER_SLAVE_CONFIG);
        } catch (IOException e) {
            e.printStackTrace();
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
    }


    /**
     * Execute insert, update, delete action.
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

            //insert an item
            List insertParams = Arrays.asList(3, "##-####", "##-####");
            ResultSet rs = JDBCDataSourceUtil.insert(connection, INSERT_SQL, insertParams);
            rs.next();
            Long id = rs.getLong(1);

            //update an item
            List updateParams = Arrays.asList("new", "new", id);
            JDBCDataSourceUtil.update(connection, UPDATE_SQL, updateParams);

            //delete an item
            List deleteParams = Arrays.asList(3, id);
            JDBCDataSourceUtil.delete(connection, DELETE_SQL, deleteParams);
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

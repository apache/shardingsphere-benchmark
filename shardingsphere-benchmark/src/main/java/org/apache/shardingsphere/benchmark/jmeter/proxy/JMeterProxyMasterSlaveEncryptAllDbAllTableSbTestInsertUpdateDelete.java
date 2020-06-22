package org.apache.shardingsphere.benchmark.jmeter.proxy;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

/**
 * Refactor old case test_plan/ms_enc_sharding_new/sp_all.jmx
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
public class JMeterProxyMasterSlaveEncryptAllDbAllTableSbTestInsertUpdateDelete extends AbstractJavaSamplerClient {

    public Connection connection;

    public static DataSource dataSource;

    public static final int DB_PORT = 3307;

    public static final String DB_PASSWORD = "root";

    public static final String DB_USER_NAME = "root";

    public static final String DB_HOST = "10.222.16.178";

    public static final String DB_NAME = "sharding_db";

    public static final String TABLE_NAME = "sbtest";

    public static final String INSERT_SQL = "INSERT INTO " + TABLE_NAME + " (k,c,pad) VALUES (?,?,?)";

    public static final String UPDATE_SQL = "update " + TABLE_NAME + " set c=?,pad =? where id=? and k=3";

    public static final String DELETE_SQL = "delete from " + TABLE_NAME + "  where k=? and id=?";


    // Init dataSource.
    static {
        dataSource = JDBCDataSourceUtil.initDb(DB_NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
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
        results.setSampleLabel("SPEncryptShardingAll");
        results.sampleStart();

        try {

            //Insert record
            List insertParams = Arrays.asList(3, "##-####", "##-####");
            ResultSet result = JDBCDataSourceUtil.insert(connection, INSERT_SQL, insertParams);
            result.next();
            Long id = result.getLong(1);

            //update record
            List updateParams = Arrays.asList("new", "new", id);;
            JDBCDataSourceUtil.update(connection, UPDATE_SQL, updateParams);

            //Delete record
            List deleteParams = Arrays.asList(3, id);
            JDBCDataSourceUtil.delete(connection, DELETE_SQL, deleteParams);

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

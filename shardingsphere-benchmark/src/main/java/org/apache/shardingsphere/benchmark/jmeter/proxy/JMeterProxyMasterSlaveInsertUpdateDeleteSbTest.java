package org.apache.shardingsphere.benchmark.jmeter.proxy;

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
 * Refactor old case test_plan/master_slave_new/sp_all_new.jmx
 */
public class JMeterProxyMasterSlaveInsertUpdateDeleteSbTest extends AbstractJavaSamplerClient {
    public static final int DB_PORT = 3307;

    public static final String DB_PASSWORD = "root";

    public static final String DB_USER_NAME = "root";

    public static final String DB_HOST = "10.222.16.178";

    public static final String DB_DATASOURCE_NAME = "sharding_db";

    public static final String TABLE_NAME = "sbtest3";

    public static final String INSERT_SQL = "INSERT INTO " + TABLE_NAME + " (k,c,pad) VALUES (?,?,?)";

    public static final String UPDATE_SQL = "update " + TABLE_NAME + " set c=?,pad =? where id=? and k=3";

    public static final String DELETE_SQL = "delete from " + TABLE_NAME + "  where k=? and id=?";


    static {
        JDBCDataSourceUtil.initDb(DB_DATASOURCE_NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("SPPerformanceSingleTableAllnew");
        results.sampleStart();

        try {
            //Insert a record
            List insertParams = Arrays.asList(3, "##-####", "##-####");
            conn = JDBCDataSourceUtil.getDataSource(DB_DATASOURCE_NAME).getConnection();
            ResultSet result = JDBCDataSourceUtil.insert(conn, INSERT_SQL, insertParams);
            result.next();
            Long id = result.getLong(1);

            //Update a record
            List updateParams = Arrays.asList("new", "new", id);;
            JDBCDataSourceUtil.update(conn, UPDATE_SQL, updateParams);

            //Delete a record
            List deleteParams = Arrays.asList(3, id);
            JDBCDataSourceUtil.delete(conn, DELETE_SQL, deleteParams);

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
    public void setupTest(JavaSamplerContext context) {
    }
}

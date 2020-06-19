package org.apache.shardingsphere.benchmark.jmeter.jdbc;

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
 * Refactor old case test_plan/new_sharding/jdbc_all_ms.jmx
 */
public class JMeterJDBCMasterSlaveInsertUpdateDelete  extends AbstractJavaSamplerClient {

    public static final int DB_PORT = 3306;

    public static final String DB_PASSWORD = "";

    public static final String DB_USER_NAME = "root";

    public static final String TABLE_NAME = "sbtest6";

    public static final String DB_HOST = "10.222.16.97";

    public static final String DB__NAME = "baitiao_test";

    public static final String INSERT_SQL = "INSERT INTO " + TABLE_NAME + " (k,c,pad) VALUES (?,?,?)";

    public static final String DELETE_SQL = "delete from  " + TABLE_NAME + "  where k=? and id=?";

    public static final String UPDATE_SQL = "update  " + TABLE_NAME + "  set c=?,pad =? where id=? and k=1";



    static {
        JDBCDataSourceUtil.initDb(DB__NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
    }

    @Override
    public SampleResult runTest(JavaSamplerContext javaSamplerContext) {

        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("JdbcSingleTableSelect");
        results.sampleStart();
        try {

            //insert a record
            List insertParams = Arrays.asList(1, "##-####", "##-####");
            conn = JDBCDataSourceUtil.getDataSource(DB__NAME).getConnection();
            ResultSet rs = JDBCDataSourceUtil.insert(conn, INSERT_SQL, insertParams);
            rs.next();
            Long id = rs.getLong(1);

            //update a record
            List updateParams = Arrays.asList("##-####", "##-####", id);
            JDBCDataSourceUtil.update(conn, UPDATE_SQL, updateParams);

            //delete a record
            List deleteParams = Arrays.asList(1, id);
            JDBCDataSourceUtil.delete(conn, DELETE_SQL, deleteParams);
            results.setSuccessful(true);

        } catch (SQLException throwables) {

            results.setSuccessful(false);
            throwables.printStackTrace();
            return results;

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

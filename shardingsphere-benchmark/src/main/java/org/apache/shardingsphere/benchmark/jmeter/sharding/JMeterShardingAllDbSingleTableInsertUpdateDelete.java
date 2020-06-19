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
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

/**
 * Refactor old case test_plan/new_sharding/sj_all.jmx
 */
public class JMeterShardingAllDbSingleTableInsertUpdateDelete  extends AbstractJavaSamplerClient {


    public static final String TABLE_NAME = "sbtest";

    public static final String DELETE_SQL = "delete from " + TABLE_NAME + " where k=? and id=?";

    public static final String INSERT_SQL = "insert into " + TABLE_NAME + " (k,c,pad) VALUES (?,?,?)";

    public static final String UPDATE_SQL = "update " + TABLE_NAME + " set c=?,pad =? where id=? and k=3";



    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("SJPerformanceMSSelect");
        results.sampleStart();

        try {

            DataSource dataSource = ShardingJDBCDataSourceFactory.newInstance
                    (ShardingConfigType.ALL_DATABASE_SINGLE_TABLE_SHARDING_CONFIG);

            //insert an record
            List insertParams = Arrays.asList(3, "##-####", "##-####");
            conn = dataSource.getConnection();
            ResultSet rs = JDBCDataSourceUtil.insert(conn, INSERT_SQL, insertParams);
            rs.next();
            Long id = rs.getLong(1);

            //update an record
            List updateParams = Arrays.asList("new", "new", id);
            JDBCDataSourceUtil.update(conn, UPDATE_SQL, updateParams);

            //delete an record
            List deleteParams = Arrays.asList(3, id);
            JDBCDataSourceUtil.delete(conn, DELETE_SQL, deleteParams);
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
    public void setupTest(JavaSamplerContext context) {}
}

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

public class JMeterProxyShardingAllDbAllTableSelectSbTestSelect extends AbstractJavaSamplerClient {


    public static final int DB_PORT = 3307;

    public static final String DB_USER_NAME = "root";

    public static final String DB_PASSWORD = "root";

    public static final String DB_HOST = "10.222.16.178";

    public static final String DB_DATASOURCE_NAME = "sharding_db";

    public static final String TABLE_NAME = "sbtest";

    public static final String INSERT_SQL = "insert into " + TABLE_NAME + " (k,c,pad) VALUES (?,?,?)";

    public static final String UPDATE_SQL = "update " + TABLE_NAME + " set c=?,pad =? where id=? and k=3";

    public static final String SELECT_SQL = "select id,k from sbtest ignore index(`PRIMARY`) where id=1 and k=3";

    static {
        JDBCDataSourceUtil.initDb(DB_DATASOURCE_NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("SPPerformanceSelect");
        results.sampleStart();

        try {

            //select items
            conn = JDBCDataSourceUtil.getDataSource(DB_DATASOURCE_NAME).getConnection();
            JDBCDataSourceUtil.select(conn, SELECT_SQL, null);
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

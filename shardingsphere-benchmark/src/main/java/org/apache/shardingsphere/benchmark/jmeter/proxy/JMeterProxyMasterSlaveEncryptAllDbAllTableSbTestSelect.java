package org.apache.shardingsphere.benchmark.jmeter.proxy;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import service.api.entity.Iou;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Refactor old case test_plan/ms_enc_sharding_new/select_one.jmx
 */
public class JMeterProxyMasterSlaveEncryptAllDbAllTableSbTestSelect extends AbstractJavaSamplerClient {

    public static final int DB_PORT = 3307;

    public static final String DB_PASSWORD = "root";

    public static final String DB_USER_NAME = "root";

    public static final String DB_HOST = "10.222.16.178";

    public static final String TABLE_NAME = "sbtest";

    public static final String DB_DATASOURCE_NAME = "sharding_db";

    public static final String SELECT_SQL = "SELECT id,k from " + TABLE_NAME + " ignore index(`PRIMARY`) where id=1 and k=3";


    static {
        JDBCDataSourceUtil.initDb(DB_DATASOURCE_NAME, DB_HOST, DB_PORT, DB_USER_NAME, DB_PASSWORD);
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        Connection conn = null;

        SampleResult results = new SampleResult();
        results.setSampleLabel("SPEncryptShardingSelect");
        results.sampleStart();

        try {

            conn = JDBCDataSourceUtil.getDataSource(DB_DATASOURCE_NAME).getConnection();
            ResultSet rs = JDBCDataSourceUtil.select(conn, SELECT_SQL, null);
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

    public List getIou(ResultSet resultSet) throws SQLException {
        List<Iou> result = new LinkedList<>();
        while (resultSet.next()) {
            Iou iou = new Iou();
            iou.setK(resultSet.getInt(2));
            result.add(iou);
        }
        return result;
    }

    @Override
    public void setupTest(JavaSamplerContext context) {}
}


package org.apache.shardingsphere.benchmark.jmeter.common.datapreparation.jdbc.shardingmasterslaveencrypt;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

public class JMeterJDBCCommonShardingMasterSlaveEncryptClear extends JMeterBenchmarkBase {

    public static DataSource dataSource;
    static {
        dataSource = JDBCDataSourceUtil.initDb((String) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.datasource"),
                (String) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.host"), (int) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.port"),
                (String) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.username"), (String) dbConfig.get("jdbc.benchmark.fullrouting.shardingmasterslaveencrypt.ds0.password"));
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterJDBCCommonShardingMasterSlaveEncryptClear");
        results.sampleStart();
        Connection connection = null;

        try {
            connection = dataSource.getConnection();
            JDBCDataSourceUtil.delete(connection, (String) sqlConfig.get("common.jdbc.clear"), null);
        } catch (SQLException ex) {
            results.setSuccessful(false);
            ex.printStackTrace();
        } catch (Exception ex) {
            results.setSuccessful(false);
            ex.printStackTrace();
        } finally {
            try {
                connection.close();
            } catch (SQLException ex) {
                ex.printStackTrace();
            }
            results.sampleEnd();
        }
        return results;
    }
}


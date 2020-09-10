
package org.apache.shardingsphere.benchmark.jmeter.common.datapreparation.shardingsphere.masterslave;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.db.shardingjdbc.ShardingConfigType;
import org.apache.shardingsphere.benchmark.db.shardingjdbc.ShardingJDBCDataSourceFactory;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;

public class JMeterShardingSphereCommonMasterSlaveClear extends JMeterBenchmarkBase {
    
    public static DataSource dataSource;
    static {
        try {
            dataSource = ShardingJDBCDataSourceFactory.newInstance(ShardingConfigType.FULLROUTING_MASTER_SLAVE_SHARDINGJDBC_CONFIG);
        } catch (IOException ex) {
            ex.printStackTrace();
        } catch (SQLException ex) {
            ex.printStackTrace();
        }
    }


    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterShardingSphereCommonMasterSlaveClear");
        results.sampleStart();
        Connection connection = null;

        try {
            connection = dataSource.getConnection();
            JDBCDataSourceUtil.delete(connection, (String) sqlConfig.get("common.ss.clear"), null);
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


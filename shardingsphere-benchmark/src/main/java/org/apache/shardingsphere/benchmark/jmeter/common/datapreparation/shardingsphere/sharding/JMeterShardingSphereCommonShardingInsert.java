package org.apache.shardingsphere.benchmark.jmeter.common.datapreparation.shardingsphere.sharding;

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
import java.util.List;
import java.util.Random;

public class JMeterShardingSphereCommonShardingInsert extends JMeterBenchmarkBase {
    
    public static DataSource dataSource;
    public int tableCount = Integer.valueOf((String)userConfig.get("shardingsphere.sharding.table.count")).intValue();
    public Random r = new Random(1);
    static {
        try {
            dataSource = ShardingJDBCDataSourceFactory.newInstance(ShardingConfigType.FULLROUTING_SHARDING_SHARDINGJDBC_CONFIG);
        } catch (IOException ex) {
            ex.printStackTrace();
        } catch (SQLException ex) {
            ex.printStackTrace();
        }
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterShardingSphereCommonShardingInsert");
        results.sampleStart();
        Connection connection = null;

        try {
            connection = dataSource.getConnection();
            String insertSql = (String) sqlConfig.get("common.ss.insert.sql");
            List insertParams = convertParams((List) sqlConfig.get("common.ss.insert.values"), r.nextInt(tableCount));
            JDBCDataSourceUtil.insert(connection, insertSql, insertParams);
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

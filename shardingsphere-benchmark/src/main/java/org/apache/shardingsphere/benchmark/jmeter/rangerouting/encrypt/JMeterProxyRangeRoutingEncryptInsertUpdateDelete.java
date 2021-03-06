package org.apache.shardingsphere.benchmark.jmeter.rangerouting.encrypt;

import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import org.apache.shardingsphere.benchmark.db.jdbc.JDBCDataSourceUtil;
import org.apache.shardingsphere.benchmark.jmeter.JMeterBenchmarkBase;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

public class JMeterProxyRangeRoutingEncryptInsertUpdateDelete extends JMeterBenchmarkBase {

    public static DataSource dataSource;

    static {
        dataSource = JDBCDataSourceUtil.initDb((String) dbConfig.get("ss.proxy.db.datasource"),
                (String) dbConfig.get("ss.proxy.host"), (int) dbConfig.get("ss.proxy.port"),
                (String) dbConfig.get("ss.proxy.db.username"), (String) dbConfig.get("ss.proxy.db.password"));
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {

        ResultSet rs = null;
        SampleResult results = new SampleResult();
        results.setSampleLabel("JMeterProxyFullRoutingEncryptInsert");
        results.sampleStart();
        Connection connection = null;

        try {
            connection = dataSource.getConnection();

            String insertSql = (String) sqlConfig.get("ss.benchmark.rangerouting.encrypt.insert.sql");
            List insertParams = convertParams((List) sqlConfig.get("ss.benchmark.rangerouting.encrypt.insert.values"));
    
    
            String insertSqlBatch = (String) sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.insert.sql");
            int insertCount = getInsertCount(insertSqlBatch);
/*           
            List insertBatchParams = convertParams((List) sqlConfig.get("ss.benchmark.rangerouting.shardingmasterslaveencrypt.insert.values"));
            rs = JDBCDataSourceUtil.insert(connection, insertSqlBatch, insertParams);
            List batchIds = batchInsert(rs, insertCount);*/
            List batchIds = batchInsert(insertCount, connection, insertSql, insertParams);


            String updateSql = (String) sqlConfig.get("ss.benchmark.rangerouting.encrypt.update.sql");
            List updateParams = convertParams((List) sqlConfig.get("ss.benchmark.rangerouting.encrypt.update.values"));
            updateParams = appendIds(batchIds, updateParams);
            JDBCDataSourceUtil.update(connection, updateSql, updateParams);

            String deleteSql = (String) sqlConfig.get("ss.benchmark.rangerouting.encrypt.delete.sql");
            List deleteParams = convertParams((List) sqlConfig.get("ss.benchmark.rangerouting.encrypt.delete.values"));
            deleteParams = appendIds(batchIds, deleteParams);
            JDBCDataSourceUtil.delete(connection, deleteSql, deleteParams);

            results.setSuccessful(true);
        } catch (SQLException e) {
            results.setSuccessful(false);
            e.printStackTrace();
        } catch (Exception e) {
            results.setSuccessful(false);
            e.printStackTrace();
        } finally {
            results.sampleEnd();
            try {
                connection.close();
            } catch (SQLException throwables) {
                throwables.printStackTrace();
            }
        }
        return results;
    }
}

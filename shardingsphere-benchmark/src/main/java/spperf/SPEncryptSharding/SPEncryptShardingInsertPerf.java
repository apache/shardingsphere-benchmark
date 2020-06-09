package spperf.SPEncryptSharding;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.util.config.DataSourceUtil;

import java.sql.SQLException;

public class SPEncryptShardingInsertPerf extends AbstractJavaSamplerClient {
    static {
        DataSourceUtil.createDataSource("sharding_db", "10.222.16.178", 3307, "root");
    }
    
    @Override
    public Arguments getDefaultParameters() {
        return null;
    }
    
    @Override
    public void setupTest(JavaSamplerContext context) {
    }
    
    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("SPPerformanceSingleTableInsert");
        results.sampleStart();
        try {
            String insertSql = "INSERT INTO sbtest (k,c,pad) VALUES (?,?,?)";
            DataSourceUtil.insertIou(insertSql,"sharding_db");
        } catch (SQLException ex) {
            results.setSuccessful(false);
            return results;
        } finally {
            results.sampleEnd();
        }
        results.setSuccessful(true);
        return results;
    }
}
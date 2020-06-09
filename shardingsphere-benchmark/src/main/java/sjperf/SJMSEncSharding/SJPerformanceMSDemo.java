package sjperf.SJMSEncSharding;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.SJPerfService;
import service.util.config.sjperf.SJPerfDataSourceUtil;
import service.util.config.sjperf.SJPerfDataSourceOp;
import sjperf.v3.SQLStatement;

import javax.sql.DataSource;
import java.sql.SQLException;

public class SJPerformanceMSDemo extends AbstractJavaSamplerClient {
    public static final String INSERT_SQL_MASTER_SLAVE = SQLStatement.INSERT_SQL_SHARDING_DEMO.getValue();
    public static SJPerfService sjPerfService;
    private  DataSource dataSource;
    
    static {
        try {
            sjPerfService = new SJPerfService(SJPerfDataSourceOp.CreateMSEncShardingDataSource());
        } catch (final SQLException ignore) {
        }
    }
    
    @Override
    public void setupTest(JavaSamplerContext context) {
        dataSource=sjPerfService.dataSource;
    }
    
    @Override
    public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        
        SampleResult results = new SampleResult();
        results.setSampleLabel("SJPerformanceMSInsertDemo");
        results.sampleStart();
        try {
            SJPerfDataSourceUtil.insertDemo(INSERT_SQL_MASTER_SLAVE,dataSource);
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

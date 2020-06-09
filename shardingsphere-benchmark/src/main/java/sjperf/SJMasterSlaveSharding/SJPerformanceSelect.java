package sjperf.SJMasterSlaveSharding;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.SJPerfService;
import service.util.config.sjperf.SJPerfDataSourceOp;
import service.util.config.sjperf.SJPerfDataSourceUtil;
import sjperf.v3.SQLStatement;

import javax.sql.DataSource;
import java.sql.SQLException;

public class SJPerformanceSelect extends AbstractJavaSamplerClient {
    public static final String SELECT_SQL_MASTER_SLAVE = "select id from sbtest group by id limit 10";
    public static SJPerfService sjPerfService;
    public DataSource dataSource;
    
    static {
        try {
            sjPerfService = new SJPerfService(SJPerfDataSourceOp.CreateMSShardingDataSource());
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
        results.setSampleLabel("SJPerformanceMSSelect");
        results.sampleStart();
        try {
            SJPerfDataSourceUtil.getSelect(SELECT_SQL_MASTER_SLAVE,dataSource);
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


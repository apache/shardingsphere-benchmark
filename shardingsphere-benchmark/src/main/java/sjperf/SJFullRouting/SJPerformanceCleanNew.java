package sjperf.SJFullRouting;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.SJPerfService;
import service.util.config.sjperf.SJPerfDataSourceOp;
import service.util.config.sjperf.SJPerfDataSourceUtil;
import sjperf.v3.SQLStatement;

import javax.sql.DataSource;
import java.sql.SQLException;

public class SJPerformanceCleanNew extends AbstractJavaSamplerClient {
    public static final String SELECT_SQL_MASTER_SLAVE = SQLStatement.SELECT_SQL_SHARDING.getValue();
    public static SJPerfService sjPerfService;
    public DataSource dataSource;
    
    static {
        try {
            sjPerfService = new SJPerfService(SJPerfDataSourceOp.CreateShardingNewDataSource());
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
        results.setSampleLabel("SJPerformancecleanclean");
        results.sampleStart();
        try {
            SJPerfDataSourceUtil.clean("truncate table sbtest",dataSource);
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

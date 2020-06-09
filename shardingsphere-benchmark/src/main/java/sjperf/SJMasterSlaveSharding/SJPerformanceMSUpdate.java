package sjperf.SJMasterSlaveSharding;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.SJPerfService;
import service.util.config.sjperf.SJPerfDataSourceUtil;
import service.util.config.sjperf.SJPerfDataSourceOp;
import sjperf.v3.SQLStatement;

import javax.sql.DataSource;
import java.sql.SQLException;

/**
 * sharding jdbc performance for master slave update
 */
public class SJPerformanceMSUpdate extends AbstractJavaSamplerClient {
    public static final String UPDATE_SQL_MASTER_SLAVE = SQLStatement.UPDATE_SQL_SHARDING.getValue();
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
        results.setSampleLabel("SJPerformanceMSUpdate");
        results.sampleStart();
        try {
            SJPerfDataSourceUtil.updateStmt(UPDATE_SQL_MASTER_SLAVE,dataSource);
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

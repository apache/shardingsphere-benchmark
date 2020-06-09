package sjperf.SJSingleRouting;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.SJPerfService;
import service.util.config.sjperf.SJPerfDataSourceOp;
import service.util.config.sjperf.SJPerfDataSourceUtil;

import java.sql.SQLException;

public class SJSingleRoutingInsertPerf extends AbstractJavaSamplerClient {
    public static final String INSERT_SQL_SINGLE_ROUTING = "insert into t_order(user_id,order_id) values(9,13)";
    public static SJPerfService sjPerfService;
    static {
        try {
            sjPerfService = new SJPerfService(SJPerfDataSourceOp.CreateDataSource());
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
    @Override
    public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        
        SampleResult results = new SampleResult();
        results.setSampleLabel("SSJDBCSingleRoutingInsertPerformance");
        results.sampleStart();
        try {
            SJPerfDataSourceUtil.insert(INSERT_SQL_SINGLE_ROUTING,sjPerfService.dataSource);
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


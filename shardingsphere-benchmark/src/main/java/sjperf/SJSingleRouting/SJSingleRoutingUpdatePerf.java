package sjperf.SJSingleRouting;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.SJPerfService;
import service.util.config.sjperf.SJPerfDataSourceOp;
import service.util.config.sjperf.SJPerfDataSourceUtil;

import java.sql.SQLException;

public class SJSingleRoutingUpdatePerf extends AbstractJavaSamplerClient {
    public static final String UPDATE_SQL_SINGLE_ROUTING = "update t_order set user_id=4,order_id=3 where user_id=9 and order_id=13";
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
        results.setSampleLabel("SSJDBCSingleRoutingUpdatePerformance");
        results.sampleStart();
        try {
            SJPerfDataSourceUtil.updateStmt(UPDATE_SQL_SINGLE_ROUTING,sjPerfService.dataSource);
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

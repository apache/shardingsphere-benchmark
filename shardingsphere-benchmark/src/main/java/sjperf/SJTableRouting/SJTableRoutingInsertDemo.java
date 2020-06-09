package sjperf.SJTableRouting;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.SJPerfService;
import service.util.config.sjperf.SJPerfDataSourceOp;
import service.util.config.sjperf.SJPerfDataSourceUtil;

import java.sql.SQLException;

public class SJTableRoutingInsertDemo extends AbstractJavaSamplerClient {
    public static final String INSERT_SQL_TABLE_ROUTING = "INSERT INTO sbtest (id,k,c,pad) VALUES (?,?,?,?)";
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
        results.setSampleLabel("SSJDBCTableRoutingInsertPerformance");
        results.sampleStart();
        try {
            SJPerfDataSourceUtil.insertDemo(INSERT_SQL_TABLE_ROUTING,sjPerfService.dataSource);
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


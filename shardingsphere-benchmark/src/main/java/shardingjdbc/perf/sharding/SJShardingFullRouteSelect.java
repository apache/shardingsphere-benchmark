package shardingjdbc.perf.sharding;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import perfstmt.ShardingPerfStmt;
import service.util.config.SJDataSourceFactory;
import service.util.config.SJDataSourceUtil;

import javax.sql.DataSource;
import java.sql.SQLException;

/**
 * Sharding-Jdbc sharding full route select performance with ss dev branch.
 */
public class SJShardingFullRouteSelect extends AbstractJavaSamplerClient {
    private static final String FULL_ROUTE_SELECT_SHARDING = ShardingPerfStmt.FULL_ROUTE_SELECT.getValue();
    
    private static DataSource dataSource;
    
    static {
        try {
            dataSource = SJDataSourceFactory.createShardingDataSource();
        } catch (final SQLException ignore) {
        }
    }
    
    /**
     * run test.
     * @param javaSamplerContext context
     * @return res
     */
    @Override
    public SampleResult runTest(JavaSamplerContext javaSamplerContext) {
        
        SampleResult results = new SampleResult();
        results.setSampleLabel("SJShardingFullRouteSelect");
        results.sampleStart();
        try {
            SJDataSourceUtil.getSelect(FULL_ROUTE_SELECT_SHARDING, dataSource);
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




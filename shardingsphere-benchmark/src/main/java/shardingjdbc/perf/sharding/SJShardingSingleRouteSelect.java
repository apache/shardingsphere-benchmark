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
 * Sharding-Jdbc sharding single route select performance with ss dev branch.
 */
public class SJShardingSingleRouteSelect extends AbstractJavaSamplerClient {
    private static final String SINGLE_ROUTE_SELECT_SHARDING = ShardingPerfStmt.SINGLE_ROUTE_SELECT.getValue();
    
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
        results.setSampleLabel("SJShardingSingleRouteSelect");
        results.sampleStart();
        try {
            SJDataSourceUtil.getSelect(SINGLE_ROUTE_SELECT_SHARDING, dataSource);
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




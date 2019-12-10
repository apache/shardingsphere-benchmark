package shardingproxy.perf.sharding;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import perfstmt.ShardingPerfStmt;
import service.util.config.SPDataSourceUtil;

import java.sql.SQLException;

/**
 *  single route select performance for Sharding-Proxy according to different yaml configuration.
 */
public class SPShardingSingleRouteSelect extends AbstractJavaSamplerClient {
    private static final String SINGLE_ROUTE_SELECT_STMT = ShardingPerfStmt.SINGLE_ROUTE_SELECT.getValue();
    
    static {
        SPDataSourceUtil.createDataSource("###", "sharding_db", "###", 3307, "###");
    }
    
    /**
     * get default params.
     * @return null
     */
    @Override
    public Arguments getDefaultParameters() {
        return null;
    }
    
    /**
     * setup.
     * @param context context
     */
    @Override
    public void setupTest(JavaSamplerContext context) {
    }
    
    /**
     * run test.
     * @param context context
     * @return sample res
     */
    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("SPShardingSingleRouteSelect");
        results.sampleStart();
        try {
            SPDataSourceUtil.getIou(SINGLE_ROUTE_SELECT_STMT, "sharding_db");
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



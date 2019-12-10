package shardingproxy.perf.sharding;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.util.config.SPDataSourceUtil;

import java.sql.SQLException;

/**
 * Insert+Update+Delete operation performance for Sharding-Proxy according to different yaml configuration.
 */
public class SPShardingWrite extends AbstractJavaSamplerClient {
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
        results.setSampleLabel("SPShardingWrite");
        results.sampleStart();
        try {
            SPDataSourceUtil.writeOp("sharding_db");
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
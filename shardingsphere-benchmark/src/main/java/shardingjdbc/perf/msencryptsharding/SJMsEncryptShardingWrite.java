package shardingjdbc.perf.msencryptsharding;

import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.util.config.SJDataSourceFactory;
import service.util.config.SJDataSourceUtil;

import javax.sql.DataSource;
import java.sql.SQLException;

/**
 * Sharding-Jdbc Insert+Update+Delete operation performance test for master slave & encrypt & sharding
 */
public class SJMsEncryptShardingWrite extends AbstractJavaSamplerClient {
    
    private static DataSource dataSource;
    
    static {
        try {
            dataSource = SJDataSourceFactory.createMSEncShardingDataSource();
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
        results.setSampleLabel("SJMsEncryptShardingWrite");
        results.sampleStart();
        try {
            SJDataSourceUtil.writeOp(dataSource);
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
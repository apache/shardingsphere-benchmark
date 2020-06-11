package spperf;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.util.config.DataSourceUtil;

import java.sql.SQLException;

/**
 * sharding proxy insert demo data
 * @author nancyzrh
 */
public class SPPerformanceSingleTableDemo extends AbstractJavaSamplerClient {
    static {
        DataSourceUtil.createDataSource("test", "####", 3307, "###");
    }

    @Override
    public Arguments getDefaultParameters() {
        return null;
    }

    @Override
    public void setupTest(JavaSamplerContext context) {
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("SPPerformanceSingleTableDemo");
        results.sampleStart();
        try {
            String insertSql = "INSERT INTO test (id,k,c,pad) VALUES (?,?,?,?)";
            DataSourceUtil.insertDemo(insertSql,"test");
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

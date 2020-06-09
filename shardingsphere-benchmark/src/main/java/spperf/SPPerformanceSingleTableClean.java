package spperf;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.util.config.DataSourceUtil;

import java.sql.SQLException;

/**
 * sharding proxy single table clean up
 * @author nancyzrh
 */
public class SPPerformanceSingleTableClean extends AbstractJavaSamplerClient {
    static {
        DataSourceUtil.createDataSource("test", "####", 3307, "####");
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
        results.setSampleLabel("SPPerformanceSingleTableClean");
        results.sampleStart();
        try {
            String deleteSql = "delete from test";
            DataSourceUtil.clean(deleteSql,"test");
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

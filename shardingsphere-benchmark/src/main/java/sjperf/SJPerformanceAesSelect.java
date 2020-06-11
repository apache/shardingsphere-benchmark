package sjperf;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.util.config.AesDataSourceUtil;
import service.util.config.DataSourceUtil;

import java.sql.SQLException;

/**
 *  for aes performance select performance
 */
public class SJPerformanceAesSelect extends AbstractJavaSamplerClient {
    static {
        AesDataSourceUtil.createDataSource("test", "####", 3306, "####");
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
        results.setSampleLabel("SJPerforanceAesSelect");
        results.sampleStart();
        try {
            String insertSql = "SELECT id,k from test where id=1 and k=1";
            AesDataSourceUtil.getIou(insertSql,"test");
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

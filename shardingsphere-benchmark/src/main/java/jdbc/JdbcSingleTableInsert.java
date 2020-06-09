package jdbc;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.util.config.DataSourceUtil;

import java.sql.SQLException;

/**
 *  for jdbc single table insert performance
 */
public class JdbcSingleTableInsert extends AbstractJavaSamplerClient {
    static {
        DataSourceUtil.createDataSource("test", "#####", 3306, "####");
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
        results.setSampleLabel("JdbcSingleTableInsert");
        results.sampleStart();
        try {
            String insertSql = "INSERT INTO test (k,c,pad) VALUES (?,?,?)";
            DataSourceUtil.insertIou(insertSql,"test");
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


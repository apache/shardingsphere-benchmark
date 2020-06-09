package sjperf;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.CommonService;
import service.config.MasterSlaveConfiguration;
import service.repository.jdbc.JDBCOrderItemRepositoryImpl;
import service.repository.jdbc.JDBCOrderRepositoryImpl;
import service.repository.service.RawPojoService;
import service.util.config.DataSourceUtil;

import javax.sql.DataSource;
import java.sql.SQLException;

/**
 * sharding jdbc performance for master slave select
 */
public class SJPerformanceMSSelect extends AbstractJavaSamplerClient {
    private static DataSource dataSource;

    private CommonService commonService;

    static {
        DataSourceUtil.createDataSource("master_ds", "#####", 3306, "#####");
        try {
            dataSource = new MasterSlaveConfiguration().createDataSource();
        } catch (SQLException ignore) {
        }
    }

    @Override
    public Arguments getDefaultParameters() {
        return null;
    }

    @Override
    public void setupTest(JavaSamplerContext context) {
        commonService = new RawPojoService(new JDBCOrderRepositoryImpl(dataSource), new JDBCOrderItemRepositoryImpl(dataSource));
    }

    @Override
    public SampleResult runTest(JavaSamplerContext context) {
        SampleResult results = new SampleResult();
        results.setSampleLabel("SJPerformanceMasterSlaveSelect");
        results.sampleStart();
        try {
            commonService.processSuccess(false, "SELECT orderId FROM t_order limit 10");
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

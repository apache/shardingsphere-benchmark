package sjperf;

import org.apache.jmeter.config.Arguments;
import org.apache.jmeter.protocol.java.sampler.AbstractJavaSamplerClient;
import org.apache.jmeter.protocol.java.sampler.JavaSamplerContext;
import org.apache.jmeter.samplers.SampleResult;
import service.api.service.CommonService;
import service.config.ShardingDatabasesAndTablesConfigurationPrecise;
import service.repository.jdbc.JDBCOrderItemRepositoryImpl;
import service.repository.jdbc.JDBCOrderRepositoryImpl;
import service.repository.service.RawPojoService;
import service.util.config.DataSourceUtil;

import javax.sql.DataSource;
import java.sql.SQLException;

/**
 * sharding jdbc single table select
 */
public class SJPerformanceSingleTable extends AbstractJavaSamplerClient {

    private static DataSource dataSource;

    private CommonService commonService;

    static {
        DataSourceUtil.createDataSource("p_ds_0", "####", 3306, "###");
        DataSourceUtil.createDataSource("p_ds_1", "####", 3306, "####");
        try {
            dataSource = new ShardingDatabasesAndTablesConfigurationPrecise().createDataSource();
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
        results.setSampleLabel("SJPerformanceSingleTableSelect");
        results.sampleStart();
        try {
            commonService.processSuccess(false, "SELECT * FROM t_order WHERE user_id = 1 AND order_id = 1");
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


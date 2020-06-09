package service.config;

import org.apache.shardingsphere.masterslave.api.config.MasterSlaveRuleConfiguration;
import org.apache.shardingsphere.driver.api.ShardingSphereDataSourceFactory;
import org.apache.shardingsphere.infra.config.RuleConfiguration;
import org.apache.shardingsphere.masterslave.api.config.rule.MasterSlaveDataSourceRuleConfiguration;
import org.apache.shardingsphere.masterslave.rule.MasterSlaveRule;
import service.util.config.DataSourceUtil;
import service.util.config.ExampleConfiguration;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class MasterSlaveConfiguration implements ExampleConfiguration {

    private static DataSource dataSource;

    @Override
    public DataSource createDataSource() throws SQLException {
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig = new MasterSlaveDataSourceRuleConfiguration("master_slave", "master_ds", Arrays.asList("slave_ds_0", "slave_ds_1"), null);
        MasterSlaveRuleConfiguration masterSlaveRuleConfiguration = new MasterSlaveRuleConfiguration(null, Arrays.asList(masterSlaveRuleConfig));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        dataSource = ShardingSphereDataSourceFactory.createDataSource(createDataSourceMap(), Arrays.<RuleConfiguration>asList(masterSlaveRuleConfiguration), properties);
        return dataSource;
    }

    @Override
    public DataSource getDataSource() {
        return dataSource;
    }

    private Map<String, DataSource> createDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("master_ds", DataSourceUtil.getDataSource("master_ds"));
        result.put("slave_ds_0", DataSourceUtil.getDataSource("master_ds"));
        result.put("slave_ds_1", DataSourceUtil.getDataSource("master_ds"));
        return result;
    }


}

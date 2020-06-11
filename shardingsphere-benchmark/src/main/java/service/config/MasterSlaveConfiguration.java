package service.config;

import org.apache.shardingsphere.api.config.masterslave.MasterSlaveRuleConfiguration;
import org.apache.shardingsphere.shardingjdbc.api.MasterSlaveDataSourceFactory;
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
        MasterSlaveRuleConfiguration masterSlaveRuleConfig = new MasterSlaveRuleConfiguration("master_slave", "master_ds", Arrays.asList("slave_ds_0", "slave_ds_1"));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        dataSource = MasterSlaveDataSourceFactory.createDataSource(createDataSourceMap(), masterSlaveRuleConfig, properties);
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

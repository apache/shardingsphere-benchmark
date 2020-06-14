package service.config;

import org.apache.shardingsphere.sharding.api.config.ShardingRuleConfiguration;
import org.apache.shardingsphere.sharding.api.config.rule.ShardingTableRuleConfiguration;
import org.apache.shardingsphere.sharding.api.config.strategy.sharding.StandardShardingStrategyConfiguration;
import org.apache.shardingsphere.sharding.strategy.algorithm.sharding.inline.InlineShardingAlgorithm;
import org.apache.shardingsphere.driver.api.ShardingSphereDataSourceFactory;
import org.apache.shardingsphere.infra.config.RuleConfiguration;
import service.util.config.DataSourceUtil;
import service.util.config.ExampleConfiguration;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class ShardingDatabasesAndTablesConfigurationPrecise implements ExampleConfiguration {

    private static DataSource dataSource;

    @Override
    public DataSource createDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTables().add(getOrderTableRuleConfiguration());
        shardingRuleConfig.getTables().add(getOrderItemTableRuleConfiguration());
        shardingRuleConfig.getBindingTableGroups().add("t_order, t_order_item");
    
        InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProperties().setProperty("algorithm.expression", "p_ds_${user_id % 2}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProperties().setProperty("algorithm.expression", "t_order_${order_id % 2}");
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("user_id", shardingAlgorithm1));
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("order_id", shardingAlgorithm2));
        
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        dataSource = ShardingSphereDataSourceFactory.createDataSource(createDataSourceMap(), Arrays.<RuleConfiguration>asList(shardingRuleConfig), properties);
        return dataSource;
    }


    @Override
    public DataSource getDataSource() {
        return dataSource;
    }

    private static ShardingTableRuleConfiguration getOrderTableRuleConfiguration() {
        ShardingTableRuleConfiguration result = new ShardingTableRuleConfiguration("t_order", "p_ds_${0..1}.t_order_${[0, 1]}");
        return result;
    }

    private static ShardingTableRuleConfiguration getOrderItemTableRuleConfiguration() {
        ShardingTableRuleConfiguration result = new ShardingTableRuleConfiguration("t_order_item", "p_ds_${0..1}.t_order_item_${[0, 1]}");
        return result;
    }

    private static Map<String, DataSource> createDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("p_ds_0", DataSourceUtil.getDataSource("p_ds_0"));
        result.put("p_ds_1", DataSourceUtil.getDataSource("p_ds_1"));
        return result;
    }
}

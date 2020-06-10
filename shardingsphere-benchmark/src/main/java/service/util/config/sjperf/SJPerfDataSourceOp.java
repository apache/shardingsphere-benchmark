package service.util.config.sjperf;

import com.google.common.collect.Lists;
import org.apache.shardingsphere.encrypt.api.EncryptColumnRuleConfiguration;
import org.apache.shardingsphere.encrypt.api.EncryptRuleConfiguration;
import org.apache.shardingsphere.encrypt.api.EncryptTableRuleConfiguration;
import org.apache.shardingsphere.encrypt.api.EncryptorRuleConfiguration;
import org.apache.shardingsphere.api.config.masterslave.LoadBalanceStrategyConfiguration;
import org.apache.shardingsphere.api.config.masterslave.MasterSlaveRuleConfiguration;
import org.apache.shardingsphere.api.config.sharding.KeyGeneratorConfiguration;
import org.apache.shardingsphere.api.config.sharding.ShardingRuleConfiguration;
import org.apache.shardingsphere.api.config.sharding.TableRuleConfiguration;
import org.apache.shardingsphere.api.config.sharding.strategy.InlineShardingStrategyConfiguration;

import org.apache.shardingsphere.shardingjdbc.api.EncryptDataSourceFactory;
import org.apache.shardingsphere.shardingjdbc.api.MasterSlaveDataSourceFactory;
import org.apache.shardingsphere.shardingjdbc.api.ShardingDataSourceFactory;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.*;

/**
 * for different sjperf dataSource
 */
public class SJPerfDataSourceOp {
    private final Map<String, EncryptColumnRuleConfiguration> columnsEnc = new LinkedHashMap<>();
    /**
     * create full routing
     * @return  data source
     * @throws SQLException
     */

    public static DataSource CreateDataSource() throws SQLException {
        Map<String, DataSource> dataSourceMap = new HashMap<>();
        dataSourceMap.put("baitiao_test", SJPerfDataSourceUtil.createDataSource("baitiao_test", "10.222.16.156", 3306, ""));
        // dataSourceMap.put("db1", SJPerfDataSourceUtil.createDataSource("db1", "####", 3306, "####"));
        TableRuleConfiguration tableRuleConfiguration = new TableRuleConfiguration("sbtest", "baitiao_test.sbtest99");
        tableRuleConfiguration.setDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "baitiao_test"));
        tableRuleConfiguration.setTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "sbtest99"));
        //tableRuleConfiguration.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        DataSource dataSource = ShardingDataSourceFactory.createDataSource(dataSourceMap,shardingRuleConfig,properties);
        return dataSource;
    }
    public static DataSource createDataSourceTest() throws SQLException {
        Map<String, DataSource> dataSourceMap = new HashMap<>();
        dataSourceMap.put("baitiao_test", SJPerfDataSourceUtil.createDataSource("baitiao_test", "10.222.16.156", 3306, ""));
        // dataSourceMap.put("db1", SJPerfDataSourceUtil.createDataSource("db1", "####", 3306, "####"));
        TableRuleConfiguration tableRuleConfiguration = new TableRuleConfiguration("sbtest", "baitiao_test.sbtest1");
        tableRuleConfiguration.setDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "baitiao_test"));
        tableRuleConfiguration.setTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "sbtest1"));
        //tableRuleConfiguration.setKeyGeneratorConfig(getKeyGeneratorConfiguration());

        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        DataSource dataSource = ShardingDataSourceFactory.createDataSource(dataSourceMap,shardingRuleConfig,properties);
        return dataSource;
    }

    /**
     * create master slave datasource
     * @return master slave data source
     * @throws SQLException
     */
    public static DataSource CreateMSDataSource() throws SQLException {
        Map<String, DataSource> dataSourceMap = new HashMap<>();
        // dataSourceMap.put("ds_master", SJPerfDataSourceUtil.createDataSource("ds_master", "####", 3306, "###"));
        dataSourceMap.put("ds_master", SJPerfDataSourceUtil.createDataSource("baitiao_test", "10.222.16.144", 3306, ""));
        dataSourceMap.put("ds_slave0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        //dataSourceMap.put("ds_slave1", SJPerfDataSourceUtil.createDataSource("ds_slave1","####",3306,"###"));
        MasterSlaveRuleConfiguration masterSlaveRuleConfig = new MasterSlaveRuleConfiguration("ds_master_slave", "ds_master", Arrays.asList("ds_slave0"));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        DataSource dataSource = MasterSlaveDataSourceFactory.createDataSource(dataSourceMap, masterSlaveRuleConfig,properties);
        return dataSource;
    }
    public static DataSource CreateEncryptNewDataSource() throws SQLException {
        DataSource dataSource = SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,"");
        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456abc");
        EncryptorRuleConfiguration encryptorConfig = new EncryptorRuleConfiguration("AES", props);
        EncryptColumnRuleConfiguration columnConfig = new EncryptColumnRuleConfiguration("", "c", "", "aes");
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration(Collections.singletonMap("c", columnConfig));
        EncryptRuleConfiguration encryptRuleConfig = new EncryptRuleConfiguration();
        encryptRuleConfig.getEncryptors().put("aes", encryptorConfig);
        encryptRuleConfig.getTables().put("sbtest3", tableConfig);
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        DataSource encryptDatasource = EncryptDataSourceFactory.createDataSource(dataSource, encryptRuleConfig,properties);
        return encryptDatasource;
    }

    /**
     * create encrypt datasource
     * @return encrypt data source
     * @throws SQLException
     */
    public static DataSource CreateEncryptDataSource() throws SQLException {
        DataSource dataSource = SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,"");
        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456");
        EncryptorRuleConfiguration encryptorConfig = new EncryptorRuleConfiguration("aes", props);
        EncryptColumnRuleConfiguration columnConfig = new EncryptColumnRuleConfiguration("", "c", "", "aes");
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration(Collections.singletonMap("c", columnConfig));
        EncryptRuleConfiguration encryptRuleConfig = new EncryptRuleConfiguration();
        encryptRuleConfig.getEncryptors().put("aes", encryptorConfig);
        encryptRuleConfig.getTables().put("sbtest99", tableConfig);
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        DataSource encryptDatasource = EncryptDataSourceFactory.createDataSource(dataSource, encryptRuleConfig,properties);
        return encryptDatasource;
    }

    public static DataSource CreateMSDataSourcebak() throws SQLException {
        Map<String, DataSource> dataSourceMap = new HashMap<>();
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.setMasterSlaveRuleConfigs(getMSRuleConfigurations());
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createMSDataSourceMap(), shardingRuleConfig, properties);
    }
    private static List<MasterSlaveRuleConfiguration> getMSRuleConfigurations() {
        MasterSlaveRuleConfiguration masterSlaveRuleConfig = new MasterSlaveRuleConfiguration("ds_master_slave", "ds_master", Arrays.asList("ds_slave0"));
        return Lists.newArrayList(masterSlaveRuleConfig);
    }
    private static Map<String, DataSource> createMSDataSourceMap() {
        final Map<String, DataSource> result = new HashMap<>();
        result.put("ds_master", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        result.put("ds_slave0",  SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        return result;
    }
    public static DataSource CreateMSEncShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        TableRuleConfiguration tableRuleConfiguration = new TableRuleConfiguration("sbtest", "ms_ds_${0..3}.sbtest${0..1023}");
        tableRuleConfiguration.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        shardingRuleConfig.setDefaultDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ms_ds_${id % 4}"));
        shardingRuleConfig.setDefaultTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "sbtest${k % 1024}"));
        shardingRuleConfig.setMasterSlaveRuleConfigs(getMSEncRuleConfigurations());
        shardingRuleConfig.setEncryptRuleConfig(getMsEncRuleConfiguration());
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createMSEncDataSourceMap(), shardingRuleConfig, properties);
    }

    private static List<MasterSlaveRuleConfiguration> getMSEncRuleConfigurations() {
        LoadBalanceStrategyConfiguration loadBalanceStrategyConfiguration= new LoadBalanceStrategyConfiguration("ROUND_ROBIN");
        MasterSlaveRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveRuleConfiguration("ms_ds_0", "ds_0", Arrays.asList("ds_0_slave_0"),loadBalanceStrategyConfiguration);
        MasterSlaveRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveRuleConfiguration("ms_ds_1", "ds_1", Arrays.asList("ds_1_slave_1"),loadBalanceStrategyConfiguration);
        MasterSlaveRuleConfiguration masterSlaveRuleConfig3 = new MasterSlaveRuleConfiguration("ms_ds_2", "ds_2", Arrays.asList("ds_2_slave_2"),loadBalanceStrategyConfiguration);
        MasterSlaveRuleConfiguration masterSlaveRuleConfig4 = new MasterSlaveRuleConfiguration("ms_ds_3", "ds_3", Arrays.asList("ds_3_slave_3"),loadBalanceStrategyConfiguration);
        return Lists.newArrayList(masterSlaveRuleConfig1, masterSlaveRuleConfig2, masterSlaveRuleConfig3, masterSlaveRuleConfig4);
    }

    private static Map<String, DataSource> createMSEncDataSourceMap() {
        final Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.86",3306,""));
        result.put("ds_0_slave_0",  SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.86",3306,""));
        result.put("ds_1",SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_1_slave_1", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_2", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.144",3306,""));
        result.put("ds_2_slave_2",  SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.144",3306,""));
        result.put("ds_3",SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        result.put("ds_3_slave_3", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        return result;
    }


    /**
     * create ms sharding datasource
     * @return
     */
    public static DataSource CreateMSShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();

        TableRuleConfiguration tableRuleConfiguration = new TableRuleConfiguration("sbtest", "ms_ds_${0..1}.sbtest${0..1023}");
        tableRuleConfiguration.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        shardingRuleConfig.getBroadcastTables().add("t_config");
        shardingRuleConfig.setDefaultDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ms_ds_${id % 2}"));
        shardingRuleConfig.setDefaultTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "sbtest${k % 1024}"));
        shardingRuleConfig.setMasterSlaveRuleConfigs(getMasterSlaveRuleConfigurations());
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createMSsharingDataSourceMap(), shardingRuleConfig,properties);
    }

    private static List<MasterSlaveRuleConfiguration> getMasterSlaveRuleConfigurations() {
        MasterSlaveRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveRuleConfiguration("ms_ds_0", "ds_0", Arrays.asList("ds_0_slave_0"));
        MasterSlaveRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveRuleConfiguration("ms_ds_1", "ds_1", Arrays.asList("ds_1_slave_0"));
        return Lists.newArrayList(masterSlaveRuleConfig1, masterSlaveRuleConfig2);
    }
    private static Map<String, DataSource> createMSsharingDataSourceMap() {
        final Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_0_slave_0",  SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_1",SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        result.put("ds_1_slave_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        return result;
    }
    /**
     * create sharding datasource
     * @return
     * @throws
     */
    public static DataSource CreateShardingAllDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        TableRuleConfiguration tableRuleConfig = new TableRuleConfiguration("sbtest","ds_${0..3}.sbtest3");
        tableRuleConfig.setDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ds_${id % 4}"));
        tableRuleConfig.setTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "sbtest3"));
        tableRuleConfig.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfig);
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createShardingDataAllSourceMap(),shardingRuleConfig,properties);
    }

    private static Map<String, DataSource> createShardingDataAllSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_1", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_2", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_3", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        return result;
    }
    /**
     * create sharding datasource
     * @return
     * @throws
     */
    public static DataSource CreateShardingNewDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        TableRuleConfiguration tableRuleConfig = new TableRuleConfiguration("sbtest","ds_${0..3}.sbtest4");
        tableRuleConfig.setDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ds_${id % 4}"));
        tableRuleConfig.setTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "sbtest4"));
        tableRuleConfig.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfig);
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createShardingNewDataSourceMap(), shardingRuleConfig, properties);
    }

    private static Map<String, DataSource> createShardingNewDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.86",3306,""));
        result.put("ds_1", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        result.put("ds_2", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.144",3306,""));
        result.put("ds_3", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        return result;
    }

    public static DataSource CreateShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        TableRuleConfiguration tableRuleConfig = new TableRuleConfiguration("sbtest","ds_${0..3}.sbtest${0..1023}");
        tableRuleConfig.setDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ds_${id % 4}"));
        tableRuleConfig.setTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "sbtest${k % 1024}"));
        tableRuleConfig.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfig);
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        //properties.setProperty("acceptor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createShardingDataSourceMap(), shardingRuleConfig, properties);
    }

    private static Map<String, DataSource> createShardingDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_1", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_2", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_3", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        // result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.86",3306,""));
        // result.put("ds_1", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        //  result.put("ds_2", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.144",3306,""));
        //  result.put("ds_3", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        return result;
    }
    /**
     * create encrypt sharding datasource
     * @return encrypt data source
     * @throws SQLException
     */
    public static DataSource CreateEncryptShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTableRuleConfigs().add(getTableRuleConfiguration());
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        shardingRuleConfig.setDefaultDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ds_${id % 2}"));
        shardingRuleConfig.setDefaultTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "sbtest${k % 1024}"));
        shardingRuleConfig.setEncryptRuleConfig(getEncryptRuleConfiguration());
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createDataSourceMap(), shardingRuleConfig,properties);
    }
    private static TableRuleConfiguration getMSTableRuleConfiguration() {
        TableRuleConfiguration result = new TableRuleConfiguration("sbtest", "ms_ds_${0..1}.sbtest${0..1023}");
        result.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        return result;
    }
    private static TableRuleConfiguration getTableRuleConfiguration() {
        TableRuleConfiguration result = new TableRuleConfiguration("sbtest", "ds_${0..1}.sbtest${0..1023}");
        result.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        return result;
    }
    private static EncryptRuleConfiguration getEncryptRuleConfiguration() {
        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456abc");
        EncryptorRuleConfiguration encryptorConfig = new EncryptorRuleConfiguration("AES", props);
        EncryptColumnRuleConfiguration columnConfig = new EncryptColumnRuleConfiguration("", "c", "", "aes");
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration(Collections.singletonMap("c", columnConfig));
        EncryptRuleConfiguration encryptRuleConfig = new EncryptRuleConfiguration();
        encryptRuleConfig.getEncryptors().put("aes", encryptorConfig);
        encryptRuleConfig.getTables().put("sbtest", tableConfig);
        return encryptRuleConfig;
    }
    private static EncryptRuleConfiguration getMsEncRuleConfiguration() {
        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456abc");
        Map<String, EncryptColumnRuleConfiguration> columns = new LinkedHashMap<>();
        EncryptorRuleConfiguration encryptorConfig = new EncryptorRuleConfiguration("AES", props);
        EncryptColumnRuleConfiguration columnConfig = new EncryptColumnRuleConfiguration("", "c", "", "aes");
        columns.put("c",columnConfig);
        //   EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration(Collections.singletonMap("c", columnConfig));

        EncryptorRuleConfiguration encryptorConfigMd5 = new EncryptorRuleConfiguration("md5",new Properties());
        EncryptColumnRuleConfiguration columnConfigMd5 = new EncryptColumnRuleConfiguration("", "pad", "", "md5");
        columns.put("pad",columnConfigMd5);
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration(columns);
        EncryptRuleConfiguration encryptRuleConfig = new EncryptRuleConfiguration();
        encryptRuleConfig.getEncryptors().put("aes", encryptorConfig);
        encryptRuleConfig.getEncryptors().put("md5", encryptorConfigMd5);
        encryptRuleConfig.getTables().put("sbtest", tableConfig);
        return encryptRuleConfig;
    }
    private static Map<String, DataSource> createDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        result.put("ds_1", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        return result;
    }

    private static KeyGeneratorConfiguration getKeyGeneratorConfiguration() {
        return new KeyGeneratorConfiguration("SNOWFLAKE", "id", new Properties());
        //return new KeyGeneratorConfiguration("INCREMENT", "id", new Properties());
    }

}


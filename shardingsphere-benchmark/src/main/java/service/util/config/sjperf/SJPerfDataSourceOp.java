package service.util.config.sjperf;

import org.apache.shardingsphere.encrypt.api.config.EncryptRuleConfiguration;
import org.apache.shardingsphere.encrypt.api.config.rule.EncryptColumnRuleConfiguration;
import org.apache.shardingsphere.encrypt.api.config.rule.EncryptTableRuleConfiguration;
import org.apache.shardingsphere.encrypt.api.config.strategy.EncryptStrategyConfiguration;
import org.apache.shardingsphere.encrypt.api.config.strategy.impl.SPIEncryptStrategyConfiguration;
import org.apache.shardingsphere.masterslave.api.config.MasterSlaveRuleConfiguration;
import org.apache.shardingsphere.masterslave.api.config.rule.MasterSlaveDataSourceRuleConfiguration;
import org.apache.shardingsphere.masterslave.api.config.strategy.LoadBalanceStrategyConfiguration;
import org.apache.shardingsphere.masterslave.api.config.strategy.impl.SPILoadBalanceStrategyConfiguration;
import org.apache.shardingsphere.sharding.api.config.KeyGeneratorConfiguration;
import org.apache.shardingsphere.sharding.api.config.ShardingRuleConfiguration;
import org.apache.shardingsphere.sharding.api.config.ShardingTableRuleConfiguration;
import org.apache.shardingsphere.sharding.api.config.strategy.StandardShardingStrategyConfiguration;
import org.apache.shardingsphere.sharding.spi.keygen.KeyGenerateAlgorithm;
import org.apache.shardingsphere.sharding.strategy.algorithm.keygen.SnowflakeKeyGenerateAlgorithm;
import org.apache.shardingsphere.sharding.strategy.algorithm.sharding.inline.InlineShardingAlgorithm;
import org.apache.shardingsphere.driver.api.ShardingSphereDataSourceFactory;
import org.apache.shardingsphere.infra.config.RuleConfiguration;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.*;

import static java.lang.System.getProperties;

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
        ShardingTableRuleConfiguration tableRuleConfiguration = new ShardingTableRuleConfiguration("sbtest", "baitiao_test.sbtest99");
        InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProperties().setProperty("algorithm.expression", "baitiao_test");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProperties().setProperty("algorithm.expression", "sbtest99");
        tableRuleConfiguration.setTableShardingStrategy(new StandardShardingStrategyConfiguration("id", shardingAlgorithm2));
        //tableRuleConfiguration.setTableShardingStrategyConfig(new StandardShardingStrategyConfiguration("id", shardingAlgorithm2));
        //tableRuleConfiguration.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200"); 
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTables().add(tableRuleConfiguration);
        //shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("k", shardingAlgorithm1));
        //shardingRuleConfig.setDefaultDatabaseShardingStrategyConfig(new StandardShardingStrategyConfiguration("k", shardingAlgorithm1));
        DataSource dataSource = ShardingSphereDataSourceFactory.createDataSource(dataSourceMap, Collections.<RuleConfiguration>singleton(shardingRuleConfig), properties);
        return dataSource;
    }

    /**
     * create master slave datasource
     * @return master slave data source
     * @throws SQLException
     */
    public static DataSource CreateMSDataSource() throws SQLException {
        Map<String, DataSource> dataSourceMap = new HashMap<>();
        dataSourceMap.put("ds_master", SJPerfDataSourceUtil.createDataSource("baitiao_test", "10.222.16.144", 3306, ""));
        dataSourceMap.put("ds_slave0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        LoadBalanceStrategyConfiguration loadBalanceStrategyConfiguration= new SPILoadBalanceStrategyConfiguration("roundRobin", "ROUND_ROBIN", new Properties());
        MasterSlaveDataSourceRuleConfiguration dataSourceConfiguration = new MasterSlaveDataSourceRuleConfiguration(
            "ds_master_slave", "ds_master", Arrays.asList("ds_slave0"),"");
        
        MasterSlaveRuleConfiguration masterSlaveRuleConfig = new MasterSlaveRuleConfiguration(Collections.singleton(loadBalanceStrategyConfiguration), Collections.singleton(dataSourceConfiguration));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        DataSource dataSource = ShardingSphereDataSourceFactory.createDataSource(dataSourceMap, Collections.<RuleConfiguration>singleton(masterSlaveRuleConfig), properties);
        return dataSource;
    }

    /**
     * create encrypt datasource
     * @return encrypt data source
     * @throws SQLException
     */
    public static DataSource CreateEncryptDataSource() throws SQLException {
        DataSource dataSource = SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,"");
        Properties properties = new Properties();
        properties.setProperty("aes.key.value", "123456");
        EncryptColumnRuleConfiguration columnConfigAes = new EncryptColumnRuleConfiguration("", "c", "",  "","aes");
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration("sbtest99", Arrays.asList(columnConfigAes));
        Collection<EncryptStrategyConfiguration> encryptStrategyConfigurations = new LinkedList<>();
        encryptStrategyConfigurations.add(new SPIEncryptStrategyConfiguration("aes", "aes", properties));
        EncryptRuleConfiguration encryptRuleConfiguration = new EncryptRuleConfiguration(encryptStrategyConfigurations, Collections.singleton(tableConfig));
        Properties properties1 = new Properties();
        properties1.setProperty("max.connections.size.per.query", "200");
        properties1.setProperty("executor.size", "200");
        return ShardingSphereDataSourceFactory.createDataSource(dataSource, Collections.<RuleConfiguration>singleton(encryptRuleConfiguration), properties1);
    }
    
    public static DataSource CreateMSEncShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        ShardingTableRuleConfiguration tableRuleConfiguration = new ShardingTableRuleConfiguration("sbtest", "ms_ds_${0..3}.sbtest${0..1023}");
        tableRuleConfiguration.setKeyGenerator(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProperties().setProperty("algorithm.expression", "ms_ds_${id % 4}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProperties().setProperty("algorithm.expression", "sbtest${k % 1024}");
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", shardingAlgorithm1));
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("k", shardingAlgorithm2));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingSphereDataSourceFactory.createDataSource(createMSEncDataSourceMap(), Arrays.asList(shardingRuleConfig, getMSEncRuleConfigurations(), getMsEncRuleConfiguration()), properties);
    }
    
    private static MasterSlaveRuleConfiguration getMSEncRuleConfigurations() {
        LoadBalanceStrategyConfiguration loadBalanceStrategyConfiguration= new SPILoadBalanceStrategyConfiguration("roundRobin", "ROUND_ROBIN", new Properties());
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_0", "ds_0", Arrays.asList("ds_0_slave_0"),loadBalanceStrategyConfiguration.getName());
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_1", "ds_1", Arrays.asList("ds_1_slave_1"),loadBalanceStrategyConfiguration.getName());
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig3 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_2", "ds_2", Arrays.asList("ds_2_slave_2"),loadBalanceStrategyConfiguration.getName());
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig4 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_3", "ds_3", Arrays.asList("ds_3_slave_3"),loadBalanceStrategyConfiguration.getName());
        return new MasterSlaveRuleConfiguration(Collections.singleton(loadBalanceStrategyConfiguration), Arrays.asList(masterSlaveRuleConfig1, masterSlaveRuleConfig2, masterSlaveRuleConfig3, masterSlaveRuleConfig4));
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

        ShardingTableRuleConfiguration tableRuleConfiguration = new ShardingTableRuleConfiguration("sbtest", "ds_${0..1}.sbtest${0..1023}");
        tableRuleConfiguration.setKeyGenerator(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfiguration);

        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        shardingRuleConfig.getBroadcastTables().add("t_config");
    
        InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProperties().setProperty("algorithm.expression", "ds_${id % 2}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProperties().setProperty("algorithm.expression", "sbtest${k % 1024}");
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", shardingAlgorithm1));
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("k", shardingAlgorithm2));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        properties.setProperty("sql.show","true");
        return ShardingSphereDataSourceFactory.createDataSource(createMSsharingDataSourceMap(), Arrays.asList(shardingRuleConfig, getMasterSlaveRuleConfigurations()), properties);
    }
    
    private static MasterSlaveRuleConfiguration getMasterSlaveRuleConfigurations() {
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_0", "ds_0", Arrays.asList("ds_0_slave_0"), null);
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_1", "ds_1", Arrays.asList("ds_1_slave_0"), null);
        LoadBalanceStrategyConfiguration loadBalanceStrategyConfiguration= new SPILoadBalanceStrategyConfiguration("roundRobin", "ROUND_ROBIN", new Properties());

        return new MasterSlaveRuleConfiguration(Collections.singleton(loadBalanceStrategyConfiguration), Arrays.asList(masterSlaveRuleConfig1, masterSlaveRuleConfig2));
    }
    private static Map<String, DataSource> createMSsharingDataSourceMap() {
        final Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_1",SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        result.put("ds_1_slave_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        result.put("ds_0_slave_0",  SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));

        return result;
    }
    /**
     * create sharding datasource
     * @return
     * @throws
     */
    public static DataSource CreateShardingAllDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        ShardingTableRuleConfiguration tableRuleConfig = new ShardingTableRuleConfiguration("sbtest","ds_${0..3}.sbtest3");
    
        InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProperties().setProperty("algorithm.expression", "ds_${id % 4}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProperties().setProperty("algorithm.expression", "sbtest3");
        tableRuleConfig.setTableShardingStrategy(new StandardShardingStrategyConfiguration("k", shardingAlgorithm2));

        tableRuleConfig.setKeyGenerator(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfig);
    
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", shardingAlgorithm1));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingSphereDataSourceFactory.createDataSource(createShardingDataAllSourceMap(), Arrays.<RuleConfiguration>asList(shardingRuleConfig), properties);
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
        ShardingTableRuleConfiguration tableRuleConfig = new ShardingTableRuleConfiguration("sbtest","ds_${0..3}.sbtest4");

        InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProperties().setProperty("algorithm.expression", "ds_${id % 4}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProperties().setProperty("algorithm.expression", "sbtest4");
        tableRuleConfig.setTableShardingStrategy(new StandardShardingStrategyConfiguration("k", shardingAlgorithm2));
    
        tableRuleConfig.setKeyGenerator(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfig);
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", shardingAlgorithm1));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingSphereDataSourceFactory.createDataSource(createShardingNewDataSourceMap(), Arrays.<RuleConfiguration>asList(shardingRuleConfig), properties);
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
        ShardingTableRuleConfiguration tableRuleConfig = new ShardingTableRuleConfiguration("sbtest","ds_${0..3}.sbtest${0..1023}");
    
        InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProperties().setProperty("algorithm.expression", "ds_${id % 4}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProperties().setProperty("algorithm.expression", "sbtest${k % 1024}");
        tableRuleConfig.setTableShardingStrategy(new StandardShardingStrategyConfiguration("k", shardingAlgorithm2));

        tableRuleConfig.setKeyGenerator(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfig);
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", shardingAlgorithm1));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingSphereDataSourceFactory.createDataSource(createShardingDataSourceMap(), Arrays.<RuleConfiguration>asList(shardingRuleConfig), properties);
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
        shardingRuleConfig.getTables().add(getTableRuleConfiguration());
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
    
        InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProperties().setProperty("algorithm.expression", "ds_${id % 2}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProperties().setProperty("algorithm.expression", "sbtest${k % 1024}");
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", shardingAlgorithm1));
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("k", shardingAlgorithm2));
        
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingSphereDataSourceFactory.createDataSource(createDataSourceMap(), Arrays.asList(shardingRuleConfig, getEncryptRuleConfiguration()), properties);
    }
    private static ShardingTableRuleConfiguration getMSTableRuleConfiguration() {
        ShardingTableRuleConfiguration result = new ShardingTableRuleConfiguration("sbtest", "ms_ds_${0..1}.sbtest${0..1023}");
        result.setKeyGenerator(getKeyGeneratorConfiguration());
        return result;
    }
    private static ShardingTableRuleConfiguration getTableRuleConfiguration() {
        ShardingTableRuleConfiguration result = new ShardingTableRuleConfiguration("sbtest", "ds_${0..1}.sbtest${0..1023}");
        result.setKeyGenerator(getKeyGeneratorConfiguration());
        return result;
    }
    private static EncryptRuleConfiguration getEncryptRuleConfiguration() {
        Properties properties = new Properties();
        properties.setProperty("aes.key.value", "123456abc");
        EncryptColumnRuleConfiguration columnConfigAes = new EncryptColumnRuleConfiguration("", "c", "", "","aes");
        Map<String, EncryptColumnRuleConfiguration> columns = new HashMap<>();
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration("sbtest", Arrays.asList(columnConfigAes));
        Collection<EncryptStrategyConfiguration> encryptStrategyConfigurations = new LinkedList<>();
        encryptStrategyConfigurations.add(new SPIEncryptStrategyConfiguration("c_encrypt_strategy", "aes", properties));
        return new EncryptRuleConfiguration(encryptStrategyConfigurations, Collections.singleton(tableConfig));

    }
    private static EncryptRuleConfiguration getMsEncRuleConfiguration() {

        Properties properties = new Properties();
        properties.setProperty("aes.key.value", "123456abc");
        EncryptColumnRuleConfiguration columnConfigAes = new EncryptColumnRuleConfiguration("c", "c", "", "c","aes");
        EncryptColumnRuleConfiguration columnConfigMd5 = new EncryptColumnRuleConfiguration("pad", "pad", "", "pad","md5");
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration("sbtest", Arrays.asList(columnConfigMd5, columnConfigAes));
        Collection<EncryptStrategyConfiguration> encryptStrategyConfigurations = new LinkedList<>();
        encryptStrategyConfigurations.add(new SPIEncryptStrategyConfiguration("aes", "aes", properties));
        encryptStrategyConfigurations.add(new SPIEncryptStrategyConfiguration("md5", "md5", properties));
        return new EncryptRuleConfiguration(encryptStrategyConfigurations, Collections.singleton(tableConfig));

    }
    private static Map<String, DataSource> createDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        result.put("ds_1", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        return result;
    }
    private static KeyGeneratorConfiguration getKeyGeneratorConfiguration() {
        KeyGenerateAlgorithm keyGenerateAlgorithm = new SnowflakeKeyGenerateAlgorithm();
        keyGenerateAlgorithm.setProperties(getProperties());
        return new KeyGeneratorConfiguration("id", keyGenerateAlgorithm);
    }
}


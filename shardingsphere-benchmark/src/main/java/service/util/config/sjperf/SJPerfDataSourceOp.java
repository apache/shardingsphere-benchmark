package service.util.config.sjperf;

import com.google.common.collect.ImmutableMap;
import org.apache.shardingsphere.encrypt.api.config.EncryptRuleConfiguration;
import org.apache.shardingsphere.encrypt.api.config.rule.EncryptColumnRuleConfiguration;
import org.apache.shardingsphere.encrypt.api.config.rule.EncryptTableRuleConfiguration;

import org.apache.shardingsphere.infra.config.algorithm.ShardingSphereAlgorithmConfiguration;

import org.apache.shardingsphere.masterslave.api.config.MasterSlaveRuleConfiguration;

import org.apache.shardingsphere.masterslave.api.config.rule.MasterSlaveDataSourceRuleConfiguration;

import org.apache.shardingsphere.sharding.api.config.ShardingRuleConfiguration;

import org.apache.shardingsphere.sharding.api.config.rule.ShardingTableRuleConfiguration;
import org.apache.shardingsphere.sharding.api.config.strategy.keygen.KeyGenerateStrategyConfiguration;
import org.apache.shardingsphere.sharding.api.config.strategy.sharding.StandardShardingStrategyConfiguration;
import org.apache.shardingsphere.sharding.spi.KeyGenerateAlgorithm;

//import org.apache.shardingsphere.sharding.strategy.algorithm.keygen.SnowflakeKeyGenerateAlgorithm;
//import org.apache.shardingsphere.sharding.strategy.algorithm.sharding.inline.InlineShardingAlgorithm;

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
        ShardingTableRuleConfiguration tableRuleConfiguration = new ShardingTableRuleConfiguration("sbtest", "baitiao_test.sbtest99");
        /*InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProps().setProperty("algorithm.expression", "baitiao_test");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProps().setProperty("algorithm.expression", "sbtest99");*/
        tableRuleConfiguration.setTableShardingStrategy(new StandardShardingStrategyConfiguration("id", "inline"));

        //tableRuleConfiguration.setTableShardingStrategyConfig(new StandardShardingStrategyConfiguration("id", shardingAlgorithm2));
        //tableRuleConfiguration.setKeyGenerateStrategyConfig(getKeyGeneratorConfiguration());

        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200"); 
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTables().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("k", "inline"));
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

        MasterSlaveDataSourceRuleConfiguration dataSourceConfiguration = new MasterSlaveDataSourceRuleConfiguration(
            "ds_master_slave", "ds_master", Arrays.asList("ds_slave0"),"roundRobin");
        MasterSlaveRuleConfiguration masterSlaveRuleConfig = new MasterSlaveRuleConfiguration(Collections.singleton(dataSourceConfiguration), ImmutableMap.of("roundRobin", new ShardingSphereAlgorithmConfiguration("ROUND_ROBIN", new Properties())));

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
        EncryptColumnRuleConfiguration columnConfigAes = new EncryptColumnRuleConfiguration("", "c", "",  "","aes");


        Properties properties1 = new Properties();
        properties1.setProperty("max.connections.size.per.query", "200");
        properties1.setProperty("executor.size", "200");

        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456");
        EncryptColumnRuleConfiguration columnConfig = new EncryptColumnRuleConfiguration("", "c", "", "", "aes");
        ShardingSphereAlgorithmConfiguration encryptAlgorithmConfiguration = new ShardingSphereAlgorithmConfiguration("aes", props);
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration("sbtest99", Arrays.asList(columnConfigAes));
        EncryptRuleConfiguration encryptRuleConfig = new EncryptRuleConfiguration(Collections.singleton(tableConfig), ImmutableMap.of("aes", encryptAlgorithmConfiguration));



        return ShardingSphereDataSourceFactory.createDataSource(dataSource, Collections.<RuleConfiguration>singleton(encryptRuleConfig), properties1);
    }
    
    public static DataSource CreateMSEncShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        ShardingTableRuleConfiguration tableRuleConfiguration = new ShardingTableRuleConfiguration("sbtest", "ms_ds_${0..3}.sbtest${0..1023}");
        tableRuleConfiguration.setKeyGenerateStrategy(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        /*InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProps().setProperty("algorithm.expression", "ms_ds_${id % 4}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProps().setProperty("algorithm.expression", "sbtest${k % 1024}");*/
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", "inline"));
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("k", "inline"));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingSphereDataSourceFactory.createDataSource(createMSEncDataSourceMap(), Arrays.asList(shardingRuleConfig, getMSEncRuleConfigurations(), getMsEncRuleConfiguration()), properties);
    }
    
    private static MasterSlaveRuleConfiguration getMSEncRuleConfigurations() {
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_0", "ds_0", Arrays.asList("ds_0_slave_0"),"roundRobin");
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_1", "ds_1", Arrays.asList("ds_1_slave_1"),"roundRobin");
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig3 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_2", "ds_2", Arrays.asList("ds_2_slave_2"),"roundRobin");
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig4 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_3", "ds_3", Arrays.asList("ds_3_slave_3"),"roundRobin");

        return new MasterSlaveRuleConfiguration(Arrays.asList(masterSlaveRuleConfig1, masterSlaveRuleConfig2, masterSlaveRuleConfig3, masterSlaveRuleConfig4),ImmutableMap.of("roundRobin", new ShardingSphereAlgorithmConfiguration("ROUND_ROBIN", new Properties())));

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
        tableRuleConfiguration.setKeyGenerateStrategy(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfiguration);

        shardingRuleConfig.getBindingTableGroups().add("sbtest");
        shardingRuleConfig.getBroadcastTables().add("t_config");
    
        /*InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProps().setProperty("algorithm.expression", "ds_${id % 2}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProps().setProperty("algorithm.expression", "sbtest${k % 1024}");*/
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", "inline"));
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("k", "inline"));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        properties.setProperty("sql.show","true");
        return ShardingSphereDataSourceFactory.createDataSource(createMSsharingDataSourceMap(), Arrays.asList(shardingRuleConfig, getMasterSlaveRuleConfigurations()), properties);
    }
    
    private static MasterSlaveRuleConfiguration getMasterSlaveRuleConfigurations() {
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_0", "ds_0", Arrays.asList("ds_0_slave_0"), null);
        MasterSlaveDataSourceRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveDataSourceRuleConfiguration("ms_ds_1", "ds_1", Arrays.asList("ds_1_slave_0"), null);

        return new MasterSlaveRuleConfiguration( Arrays.asList(masterSlaveRuleConfig1, masterSlaveRuleConfig2), ImmutableMap.<String, ShardingSphereAlgorithmConfiguration>of("roundRobin",  new ShardingSphereAlgorithmConfiguration("ROUND_ROBIN", new Properties())));

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
    
        /*InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProps().setProperty("algorithm.expression", "ds_${id % 4}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProps().setProperty("algorithm.expression", "sbtest3");*/
        tableRuleConfig.setTableShardingStrategy(new StandardShardingStrategyConfiguration("k", "inline"));

        tableRuleConfig.setKeyGenerateStrategy(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfig);
    
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", "inline"));
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

        /*InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProps().setProperty("algorithm.expression", "ds_${id % 4}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProps().setProperty("algorithm.expression", "sbtest4");*/
        tableRuleConfig.setTableShardingStrategy(new StandardShardingStrategyConfiguration("k", "inline"));
    
        tableRuleConfig.setKeyGenerateStrategy(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfig);
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", "inline"));
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
    
        /*InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProps().setProperty("algorithm.expression", "ds_${id % 4}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProps().setProperty("algorithm.expression", "sbtest${k % 1024}");*/
        tableRuleConfig.setTableShardingStrategy(new StandardShardingStrategyConfiguration("k", "inline"));

        tableRuleConfig.setKeyGenerateStrategy(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTables().add(tableRuleConfig);
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", "inline"));
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
    
        /*InlineShardingAlgorithm shardingAlgorithm1 = new InlineShardingAlgorithm();
        shardingAlgorithm1.getProps().setProperty("algorithm.expression", "ds_${id % 2}");
        InlineShardingAlgorithm shardingAlgorithm2 = new InlineShardingAlgorithm();
        shardingAlgorithm2.getProps().setProperty("algorithm.expression", "sbtest${k % 1024}");*/
        shardingRuleConfig.setDefaultDatabaseShardingStrategy(new StandardShardingStrategyConfiguration("id", "inline"));
        shardingRuleConfig.setDefaultTableShardingStrategy(new StandardShardingStrategyConfiguration("k", ""));
        
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingSphereDataSourceFactory.createDataSource(createDataSourceMap(), Arrays.asList(shardingRuleConfig, getEncryptRuleConfiguration()), properties);
    }
    private static ShardingTableRuleConfiguration getMSTableRuleConfiguration() {
        ShardingTableRuleConfiguration result = new ShardingTableRuleConfiguration("sbtest", "ms_ds_${0..1}.sbtest${0..1023}");
        result.setKeyGenerateStrategy(getKeyGeneratorConfiguration());
        return result;
    }
    private static ShardingTableRuleConfiguration getTableRuleConfiguration() {
        ShardingTableRuleConfiguration result = new ShardingTableRuleConfiguration("sbtest", "ds_${0..1}.sbtest${0..1023}");
        result.setKeyGenerateStrategy(getKeyGeneratorConfiguration());
        return result;
    }
    private static EncryptRuleConfiguration getEncryptRuleConfiguration() {

        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456abc");
        ShardingSphereAlgorithmConfiguration encryptAlgorithmConfiguration = new ShardingSphereAlgorithmConfiguration("aes", props);


        EncryptColumnRuleConfiguration columnConfigAes = new EncryptColumnRuleConfiguration("c", "c", "c", "c","aes");
        Map<String, EncryptColumnRuleConfiguration> columns = new HashMap<>();
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration("sbtest", Arrays.asList(columnConfigAes));
        return  new EncryptRuleConfiguration(Collections.singleton(tableConfig), ImmutableMap.of("aes", encryptAlgorithmConfiguration));


    }
    private static EncryptRuleConfiguration getMsEncRuleConfiguration() {

        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456abc");
        ShardingSphereAlgorithmConfiguration encryptAlgorithmConfiguration = new ShardingSphereAlgorithmConfiguration("aes", props);

        EncryptColumnRuleConfiguration columnConfigAes = new EncryptColumnRuleConfiguration("c", "c", "", "c","aes");
        EncryptColumnRuleConfiguration columnConfigMd5 = new EncryptColumnRuleConfiguration("pad", "pad", "", "pad","md5");
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration("sbtest", Arrays.asList(columnConfigMd5, columnConfigAes));

        return new EncryptRuleConfiguration(Collections.singleton(tableConfig), ImmutableMap.of("aes", encryptAlgorithmConfiguration));

    }

    private static Map<String, DataSource> createDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.156",3306,""));
        result.put("ds_1", SJPerfDataSourceUtil.createDataSource("baitiao_test","10.222.16.97",3306,""));
        return result;
    }
    private static KeyGenerateStrategyConfiguration getKeyGeneratorConfiguration() {

        /**KeyGenerateAlgorithm keyGenerateAlgorithm = new SnowflakeKeyGenerateAlgorithm();
        Properties pros = new Properties();
        pros.setProperty("worker.id", "id");
        keyGenerateAlgorithm.setProperties(pros);
        return new KeyGeneratorConfiguration("id", keyGenerateAlgorithm);**/

        return new KeyGenerateStrategyConfiguration("id", "snowflake");
    }
}


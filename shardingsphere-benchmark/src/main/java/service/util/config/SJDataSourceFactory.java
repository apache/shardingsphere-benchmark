/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package service.util.config;

import com.google.common.collect.Lists;
import org.apache.shardingsphere.api.config.encrypt.EncryptColumnRuleConfiguration;
import org.apache.shardingsphere.api.config.encrypt.EncryptRuleConfiguration;
import org.apache.shardingsphere.api.config.encrypt.EncryptTableRuleConfiguration;
import org.apache.shardingsphere.api.config.encrypt.EncryptorRuleConfiguration;
import org.apache.shardingsphere.api.config.masterslave.LoadBalanceStrategyConfiguration;
import org.apache.shardingsphere.api.config.masterslave.MasterSlaveRuleConfiguration;
import org.apache.shardingsphere.api.config.sharding.KeyGeneratorConfiguration;
import org.apache.shardingsphere.api.config.sharding.ShardingRuleConfiguration;
import org.apache.shardingsphere.api.config.sharding.TableRuleConfiguration;
import org.apache.shardingsphere.api.config.sharding.strategy.InlineShardingStrategyConfiguration;

import org.apache.shardingsphere.shardingjdbc.api.MasterSlaveDataSourceFactory;
import org.apache.shardingsphere.shardingjdbc.api.ShardingDataSourceFactory;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.Map;
import java.util.HashMap;
import java.util.Properties;
import java.util.Arrays;
import java.util.List;
import java.util.LinkedHashMap;

/**
 * create datasource operation for Sharding-Jdbc.
 * @author nancyzrh
 */
public class SJDataSourceFactory {
    
    /**
     * create datasource for master slave.
     * @return datasource db
     * @throws SQLException ex
     */
    public static DataSource createMSDataSource() throws SQLException {
        Map<String, DataSource> dataSourceMap = new HashMap<>();
        dataSourceMap.put("ds_master", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        dataSourceMap.put("ds_slave0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        MasterSlaveRuleConfiguration masterSlaveRuleConfig = new MasterSlaveRuleConfiguration("ds_master_slave", "ds_master", Arrays.asList("ds_slave0"));
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        DataSource dataSource = MasterSlaveDataSourceFactory.createDataSource(dataSourceMap, masterSlaveRuleConfig, properties);
        return dataSource;
    }
    
    /**
     * create sharding datasource.
     * @return datasource db
     * @throws SQLException ex
     */
    public static DataSource createShardingDataSource() throws SQLException {
        TableRuleConfiguration tableRuleConfig = new TableRuleConfiguration("ssperf", "ds_${0..3}.ssperf${0..1023}");
        tableRuleConfig.setDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ds_${id % 4}"));
        tableRuleConfig.setTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "ssperf${k % 1024}"));
        tableRuleConfig.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfig);
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createShardingDataSourceMap(), shardingRuleConfig, properties);
    }
    
    /**
     * create sharding datasource map.
     * @return datasource map
     */
    private static Map<String, DataSource> createShardingDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        result.put("ds_1", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        result.put("ds_2", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        result.put("ds_3", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        return result;
    }
    
    /**
     * create datasource for master slave & encrypt & sharding scene.
     * @return datasource
     * @throws SQLException sqlexception
     */
    public static DataSource createMSEncShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        TableRuleConfiguration tableRuleConfiguration = new TableRuleConfiguration("ssperf", "ms_ds_${0..3}.test${0..1023}");
        tableRuleConfiguration.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("ssperf");
        shardingRuleConfig.setDefaultDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ms_ds_${id % 2}"));
        shardingRuleConfig.setDefaultTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "test${k % 1024}"));
        shardingRuleConfig.setMasterSlaveRuleConfigs(getMSEncRuleConfigurations());
        shardingRuleConfig.setEncryptRuleConfig(getMsEncRuleConfiguration());
        Properties properties = new Properties();
        properties.setProperty("max.connections.size.per.query", "200");
        properties.setProperty("executor.size", "200");
        return ShardingDataSourceFactory.createDataSource(createMSEncDataSourceMap(), shardingRuleConfig, properties);
    }
    
    /**
     * * get master slave configurations for master slave & encrypt & sharding.
     * @return master slave rule configuration
     */
    private static List<MasterSlaveRuleConfiguration> getMSEncRuleConfigurations() {
        LoadBalanceStrategyConfiguration loadBalanceStrategyConfiguration = new LoadBalanceStrategyConfiguration("ROUND_ROBIN");
        MasterSlaveRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveRuleConfiguration("ms_ds_0", "master_0", Arrays.asList("master_0_slave_0"), loadBalanceStrategyConfiguration);
        MasterSlaveRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveRuleConfiguration("ms_ds_1", "master_1", Arrays.asList("master_1_slave_1"), loadBalanceStrategyConfiguration);
        MasterSlaveRuleConfiguration masterSlaveRuleConfig3 = new MasterSlaveRuleConfiguration("ms_ds_2", "master_2", Arrays.asList("master_2_slave_2"), loadBalanceStrategyConfiguration);
        MasterSlaveRuleConfiguration masterSlaveRuleConfig4 = new MasterSlaveRuleConfiguration("ms_ds_3", "master_3", Arrays.asList("master_3_slave_3"), loadBalanceStrategyConfiguration);
        return Lists.newArrayList(masterSlaveRuleConfig1, masterSlaveRuleConfig2, masterSlaveRuleConfig3, masterSlaveRuleConfig4);
    }
    
    /**
     * create datasourceMap for master slave & encrypt & sharding.
     * @return datasource map
     */
    private static Map<String, DataSource> createMSEncDataSourceMap() {
        final Map<String, DataSource> result = new HashMap<>();
        result.put("master_0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, ""));
        result.put("master_0_slave_0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, ""));
        result.put("master_1", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, ""));
        result.put("master_1_slave_1", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, ""));
        result.put("master_2", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, ""));
        result.put("master_2_slave_2", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, ""));
        result.put("master_3", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, ""));
        result.put("master_3_slave_3", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, ""));
        return result;
    }
    
    /**
     * get encrypt configuration for master slave & encrypt & sharding.
     * @return encryptRuleConfiguration
     */
    private static EncryptRuleConfiguration getMsEncRuleConfiguration() {
        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456abc");
        Map<String, EncryptColumnRuleConfiguration> columns = new LinkedHashMap<>();
        EncryptorRuleConfiguration encryptorConfig = new EncryptorRuleConfiguration("AES", props);
        EncryptColumnRuleConfiguration columnConfig = new EncryptColumnRuleConfiguration("c_plain", "c", "", "aes");
        columns.put("c", columnConfig);
        EncryptorRuleConfiguration encryptorConfigMd5 = new EncryptorRuleConfiguration("md5", new Properties());
        EncryptColumnRuleConfiguration columnConfigMd5 = new EncryptColumnRuleConfiguration("", "pad", "", "md5");
        columns.put("pad", columnConfigMd5);
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration(columns);
        EncryptRuleConfiguration encryptRuleConfig = new EncryptRuleConfiguration();
        encryptRuleConfig.getEncryptors().put("aes", encryptorConfig);
        encryptRuleConfig.getEncryptors().put("md5", encryptorConfigMd5);
        encryptRuleConfig.getTables().put("test", tableConfig);
        return encryptRuleConfig;
    }
    
    /**
     * get key generator config.
     * @return keyGeneratorConfiguration
     */
    private static KeyGeneratorConfiguration getKeyGeneratorConfiguration() {
        return new KeyGeneratorConfiguration("SNOWFLAKE", "id", new Properties());
    }
}


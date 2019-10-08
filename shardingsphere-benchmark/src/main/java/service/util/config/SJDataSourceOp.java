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
import java.util.Map;
import java.util.List;
import java.util.Properties;
import java.util.HashMap;
import java.util.Collections;
import java.util.Arrays;

/**
 * create datasource operation for sharding jdbc.
 * @author nancyzrh
 */
public class SJDataSourceOp {
    
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
        DataSource dataSource = MasterSlaveDataSourceFactory.createDataSource(dataSourceMap, masterSlaveRuleConfig, new Properties());
        return dataSource;
    }

    /**
     * create encrypt datasource.
     * @return encrypt data source
     * @throws SQLException ex
     */
    public static DataSource createEncryptDataSource() throws SQLException {
        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456abc");
        EncryptorRuleConfiguration encryptorConfig = new EncryptorRuleConfiguration("AES", props);
        EncryptColumnRuleConfiguration columnConfig = new EncryptColumnRuleConfiguration("", "c", "", "aes");
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration(Collections.singletonMap("c", columnConfig));
        EncryptRuleConfiguration encryptRuleConfig = new EncryptRuleConfiguration();
        encryptRuleConfig.getEncryptors().put("aes", encryptorConfig);
        encryptRuleConfig.getTables().put("ssperf", tableConfig);
        DataSource dataSource = SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####");
        DataSource encryptDatasource = EncryptDataSourceFactory.createDataSource(dataSource, encryptRuleConfig, new Properties());
        return encryptDatasource;
    }
    
    /**
     * create ms sharding datasource.
     * @return datasource
     * @throws SQLException ex
     */
    public static DataSource createMSShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        TableRuleConfiguration tableRuleConfiguration = new TableRuleConfiguration("ssperf", "ms_ds_${0..1}.ssperf${0..1023}");
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfiguration);
        shardingRuleConfig.getBindingTableGroups().add("ssperf");
        shardingRuleConfig.setDefaultKeyGeneratorConfig(getKeyGeneratorConfiguration());
        shardingRuleConfig.setDefaultDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ds_${id % 2}"));
        shardingRuleConfig.setDefaultTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "ssperf${k % 1024}"));
        shardingRuleConfig.setMasterSlaveRuleConfigs(getMasterSlaveRuleConfigurations());
        return ShardingDataSourceFactory.createDataSource(createMSsharingDataSourceMap(), shardingRuleConfig, new Properties());
    }
    
    /**
     * get table rule config for master slave.
     * @return res
     */
    private static TableRuleConfiguration getMSTableRuleConfiguration() {
        TableRuleConfiguration result = new TableRuleConfiguration("ssperf", "ms_ds_${0..1}.ssperf${0..1023}");
        result.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        return result;
    }
    
    /**
     * get master slave rule config.
     * @return res
     */
    private static List<MasterSlaveRuleConfiguration> getMasterSlaveRuleConfigurations() {
        MasterSlaveRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveRuleConfiguration("ms_ds_0", "ds_0", Arrays.asList("ds_0_slave_0"));
        MasterSlaveRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveRuleConfiguration("ms_ds_1", "ds_1", Arrays.asList("ds_1_slave_0"));
        return Lists.newArrayList(masterSlaveRuleConfig1, masterSlaveRuleConfig2);
    }
    
    /**
     * create master slave datasource map.
     * @return datasource map
     */
    private static Map<String, DataSource> createMSsharingDataSourceMap() {
        final Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        result.put("ds_0_slave_0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        result.put("ds_1", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        result.put("ds_1_slave_0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        return result;
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
        return ShardingDataSourceFactory.createDataSource(createShardingDataSourceMap(), shardingRuleConfig, new Properties());
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
     * create encrypt sharding datasource.
     * @return encrypt datasource
     * @throws SQLException ex
     */
    public static DataSource createEncryptShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTableRuleConfigs().add(getTableRuleConfiguration());
        shardingRuleConfig.getBindingTableGroups().add("ssperf");
        shardingRuleConfig.setDefaultDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ds_${id % 2}"));
        shardingRuleConfig.setDefaultTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "ssperf${k % 1024}"));
        shardingRuleConfig.setEncryptRuleConfig(getEncryptRuleConfiguration());
        return ShardingDataSourceFactory.createDataSource(createDataSourceMap(), shardingRuleConfig, new Properties());
    }
    
    /**
     * get table rule config.
     * @return table rule res
     */
    private static TableRuleConfiguration getTableRuleConfiguration() {
        TableRuleConfiguration result = new TableRuleConfiguration("ssperf", "ds_${0..1}.ssperf${0..1023}");
        result.setKeyGeneratorConfig(getKeyGeneratorConfiguration());
        return result;
    }
    
    /**
     * get encrypt rule config.
     * @return res
     */
    private static EncryptRuleConfiguration getEncryptRuleConfiguration() {
        Properties props = new Properties();
        props.setProperty("aes.key.value", "123456abc");
        EncryptorRuleConfiguration encryptorConfig = new EncryptorRuleConfiguration("AES", props);
        EncryptColumnRuleConfiguration columnConfig = new EncryptColumnRuleConfiguration("", "c", "", "aes");
        EncryptTableRuleConfiguration tableConfig = new EncryptTableRuleConfiguration(Collections.singletonMap("c", columnConfig));
        EncryptRuleConfiguration encryptRuleConfig = new EncryptRuleConfiguration();
        encryptRuleConfig.getEncryptors().put("aes", encryptorConfig);
        encryptRuleConfig.getTables().put("ssperf", tableConfig);
        return encryptRuleConfig;
    }
    
    /**
     * create datasource map.
     * @return res
     */
    private static Map<String, DataSource> createDataSourceMap() {
        Map<String, DataSource> result = new HashMap<>();
        result.put("ds_0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        result.put("ds_1", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        return result;
    }
    
    /**
     * get key generator config.
     * @return res
     */
    private static KeyGeneratorConfiguration getKeyGeneratorConfiguration() {
        return new KeyGeneratorConfiguration("SNOWFLAKE", "id", new Properties());
    }
}


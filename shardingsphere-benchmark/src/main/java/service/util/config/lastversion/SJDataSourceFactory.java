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

package service.util.config.lastversion;

import com.google.common.collect.Lists;
import io.shardingsphere.api.config.rule.MasterSlaveRuleConfiguration;
import io.shardingsphere.api.config.rule.ShardingRuleConfiguration;
import io.shardingsphere.api.config.rule.TableRuleConfiguration;
import io.shardingsphere.api.config.strategy.InlineShardingStrategyConfiguration;
import io.shardingsphere.shardingjdbc.api.MasterSlaveDataSourceFactory;
import io.shardingsphere.shardingjdbc.api.ShardingDataSourceFactory;
import service.util.config.SJDataSourceUtil;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.Map;
import java.util.List;
import java.util.Properties;
import java.util.HashMap;
import java.util.Arrays;

import java.util.concurrent.ConcurrentHashMap;

/**
 * create datasource for sharding jdbc version 3.1.0.
 * @author nancyzrh
 */
public class SJDataSourceFactory {
    
    /**
     * create sharding datasource.
     * @return datasource
     * @throws SQLException ex
     */
    public static DataSource createShardingDataSource() throws SQLException {
        TableRuleConfiguration tableRuleConfig = new TableRuleConfiguration();
        tableRuleConfig.setLogicTable("ssperf");
        tableRuleConfig.setActualDataNodes("ds_${0..3}.ssperf${0..1023}");
        tableRuleConfig.setDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ds_${id % 4}"));
        tableRuleConfig.setTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "ssperf${k % 1024}"));
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTableRuleConfigs().add(tableRuleConfig);
        return ShardingDataSourceFactory.createDataSource(createShardingDataSourceMap(), shardingRuleConfig, new HashMap<String, Object>(), new Properties());
    }
    
    /**
     * create sharding datasource map.
     * @return res
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
     * create master slave sharding datasource.
     * @return datasource
     * @throws SQLException ex
     */
    public static DataSource createMSShardingDataSource() throws SQLException {
        ShardingRuleConfiguration shardingRuleConfig = new ShardingRuleConfiguration();
        shardingRuleConfig.getTableRuleConfigs().add(getTableRuleConfiguration());
        shardingRuleConfig.getBindingTableGroups().add("ssperf");
        shardingRuleConfig.setDefaultDatabaseShardingStrategyConfig(new InlineShardingStrategyConfiguration("id", "ms_ds_${id % 2}"));
        shardingRuleConfig.setDefaultTableShardingStrategyConfig(new InlineShardingStrategyConfiguration("k", "ssperf${k % 1024}"));
        shardingRuleConfig.setMasterSlaveRuleConfigs(getMasterSlaveRuleConfigurations());
        return ShardingDataSourceFactory.createDataSource(createMSsharingDataSourceMap(), shardingRuleConfig, new HashMap<String, Object>(), new Properties());
    }
    
    /**
     * get table rule.
     * @return res
     */
    private static TableRuleConfiguration getTableRuleConfiguration() {
        TableRuleConfiguration result = new TableRuleConfiguration();
        result.setLogicTable("ssperf");
        result.setActualDataNodes("ms_ds_${0..1}.ssperf${0..1023}");
        result.setKeyGeneratorColumnName("id");
        return result;
    }
    
    /**
     * get master slave rule.
     * @return res
     */
    private static List<MasterSlaveRuleConfiguration> getMasterSlaveRuleConfigurations() {
        MasterSlaveRuleConfiguration masterSlaveRuleConfig1 = new MasterSlaveRuleConfiguration();
        masterSlaveRuleConfig1.setName("ms_ds_0");
        masterSlaveRuleConfig1.setMasterDataSourceName("ds_0");
        masterSlaveRuleConfig1.setSlaveDataSourceNames(Arrays.asList("ds_0_slave_0"));
        MasterSlaveRuleConfiguration masterSlaveRuleConfig2 = new MasterSlaveRuleConfiguration();
        masterSlaveRuleConfig2.setName("ms_ds_1");
        masterSlaveRuleConfig2.setMasterDataSourceName("ds_1");
        masterSlaveRuleConfig2.setSlaveDataSourceNames(Arrays.asList("ds_1_slave_0"));
        return Lists.newArrayList(masterSlaveRuleConfig1, masterSlaveRuleConfig2);
    }
    
    /**
     * create ms sharding datasource map.
     * @return res
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
     * create master slave datasource.
     * @return master slave data source
     * @throws SQLException ex
     */
    public static DataSource createMSDataSource() throws SQLException {
        Map<String, DataSource> dataSourceMap = new HashMap<>();
        dataSourceMap.put("ds_master", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        dataSourceMap.put("ds_slave0", SJDataSourceUtil.createDataSource("###", "sharding_db", "####", 3306, "####"));
        MasterSlaveRuleConfiguration masterSlaveRuleConfig = new MasterSlaveRuleConfiguration();
        masterSlaveRuleConfig.setName("ds_master_slave");
        masterSlaveRuleConfig.setMasterDataSourceName("ds_master");
        masterSlaveRuleConfig.setSlaveDataSourceNames(Arrays.asList("ds_slave0"));
        DataSource dataSource = MasterSlaveDataSourceFactory.createDataSource(dataSourceMap, masterSlaveRuleConfig, new ConcurrentHashMap(), new Properties());
        return dataSource;
    }
    
}

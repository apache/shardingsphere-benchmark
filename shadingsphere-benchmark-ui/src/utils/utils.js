import API from './api'

export default {
  getMysqlVsShardingData: () =>
    API.get(`/data_json/mysql_vs_shardingproxy.json`),
  getShardingProxyMasterSlaveShardingData: () =>
    API.get(`/data_json/sharding_proxy_master_slave_sharding.json`),
  getShardingProxyMasterSlaveData: () =>
    API.get(`/data_json/sharding_proxy_master_slave.json`),
  getShardingProxySingleDatabaseSingleTableData: () =>
    API.get(`/data_json/sharding_proxy_single_database_single_table.json`),
  getShardingjdbcVsShardingproxyEncrypt: () =>
    API.get(`/data_json/shardingjdbc_vs_shardingproxy_encrypt.json`),
  getShardingjdbcVsShardingproxyShardingEncrypt: () =>
    API.get(`/data_json/shardingjdbc_vs_shardingproxy_sharding_encrypt.json`)
}

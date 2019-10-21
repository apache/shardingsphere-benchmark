import API from './api'

export default {
  getMysqlVsShardingData: () =>
    API.get(`/data_json/mysql_vs_sharding-proxy.json`),
  getShardingProxyMasterSlaveShardingData: () =>
    API.get(`/data_json/sharding-proxy_master_slave_sharding.json`),
  getShardingProxyMasterSlaveData: () =>
    API.get(`/data_json/sharding-proxy_master_slave.json`),
  getShardingProxySingleDatabaseSingleTableData: () =>
    API.get(`/data_json/sharding-proxy_single_database_single_table.json`),
  getShardingjdbcVsShardingproxyEncrypt: () =>
    API.get(`/data_json/sharding-jdbc_vs_sharding-proxy_encrypt.json`),
  getShardingjdbcVsShardingproxyShardingEncrypt: () =>
    API.get(`/data_json/sharding-jdbc_vs_sharding-proxy_sharding_encrypt.json`)
}

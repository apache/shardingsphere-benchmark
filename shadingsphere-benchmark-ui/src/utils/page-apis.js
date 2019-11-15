import API from './api'

export default {
  getFileList: () => API.get(`/jtl_json/file_list.json`),
  getEncryptShardingTestData: () =>
    API.get(`/jtl_json/encrypt_sharding_test.json`),
  getLossTestData: () => API.get(`/jtl_json/loss_test.json`),
  getEncryptTestData: () => API.get(`/jtl_json/encrypt_test.json`),
  getMasterSlaveShardingTestData: () =>
    API.get(`/jtl_json/master_slave_sharding_test.json`),
  getMasterSlaveTest: () => API.get(`/jtl_json/master_slave_test.json`),
  getShardingTest: () => API.get(`/jtl_json/sharding_test.json`),
  getPromotionTest: () => API.get(`/jtl_json/promotion_test.json`)
}

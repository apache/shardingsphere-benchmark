import API from './api'

export default {
  getFileList: () => API.get(`/data_json/file_list.json`),
  getEncryptShardingTestData: () =>
    API.get(`/data_json/encrypt_sharding_test.json`),
  getLossTestData: () => API.get(`/data_json/loss_test.json`),
  getEncryptTestData: () => API.get(`/data_json/encrypt_test.json`),
  getMasterSlaveShardingTestData: () =>
    API.get(`/data_json/master_slave_sharding_test.json`),
  getMasterSlaveTest: () => API.get(`/data_json/master_slave_test.json`),
  getShardingTest: () => API.get(`/data_json/sharding_test.json`)
}

import axios from 'axios'
import { Notice } from 'iview'
const HOST =
  'https://raw.githubusercontent.com/apache/incubator-shardingsphere-benchmark/master/report'

axios.defaults.retry = 3
axios.defaults.retryDelay = 2000

axios.interceptors.response.use(undefined, err => {
  var config = err.config
  if (!config || !config.retry) return Promise.reject(err)

  config.__retryCount = config.__retryCount || 0

  if (config.__retryCount >= config.retry) {
    return Promise.reject(err)
  }

  config.__retryCount += 1

  var backoff = new Promise(resolve => {
    setTimeout(() => {
      resolve()
    }, config.retryDelay || 1000)
  })

  return backoff.then(() => {
    return axios(config)
  })
})

function ajax(url, type, options) {
  return new Promise((resolve, reject) => {
    axios({
      method: type,
      url: HOST + url,
      timeout: 3000,
      responseType: 'json'
    })
      .then(
        result => {
          const { data, status } = result
          if (status === 200) {
            resolve(data)
          }
        },
        error => {
          const { message } = error
          if (message.includes('timeout')) {
            Notice.info({
              title: 'Prompt information',
              desc: 'please try again later！'
            })
          }
          reject(error)
        }
      )
      .catch((error) => {
        Notice.error({
          title: 'Prompt information',
          description: error
        })
        reject({
          error: true,
          msg: error
        })
      })
  })
}

const config = {
  get(url, options, config) {
    return new Promise((resolve, reject) => {
      ajax(url, 'get', options, config).then(
        data => {
          resolve(data)
        },
        error => {
          reject(error)
        }
      )
    })
  }
}

export default config

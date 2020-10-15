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

import axios from 'axios'
import { Notice } from 'iview'

const HOST =
  'https://gitbox.apache.org/repos/asf?p=shardingsphere-benchmark.git;a=blob_plain;f=report'

const TAIL =
  ';hb=HEAD'

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
      url: HOST + url + TAIL,
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

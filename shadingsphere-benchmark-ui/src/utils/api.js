import $ from 'jquery'

const HOST =
  'https://raw.githubusercontent.com/apache/incubator-shardingsphere-benchmark/master/report'

function ajax(url, type) {
  return new Promise((resolve, reject) => {
    $.getJSON(HOST + url, function(data, type, xhr) {
      if (xhr.status === 200) {
        resolve(data)
      } else {
        reject({
          error: true,
          msg: xhr.status
        })
      }
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

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

import moment from 'moment'
const mountedMixin = {
  data() {
    return {
      legend: {},
      series: {},
      desc: {},
      xAxis: {},
      loading: false,
      columns: [
        {
          title: 'MySQL verison',
          key: 'mysqlVerison'
        },
        {
          title: 'table description',
          key: 'tableDescription'
        },
        {
          title: 'encrypt rule',
          key: 'encryptRule',
          render: (h, params) => {
            const rows = params.row.encryptRule.split('\n')
            const html = []
            for (const v of rows) {
              html.push(h('div', v))
            }
            return h('div', html)
          }
        },
        {
          title: 'master slaveRule',
          key: 'masterSlaveRule',
          render: (h, params) => {
            const rows = params.row.masterSlaveRule.split('\n')
            const html = []
            for (const v of rows) {
              html.push(h('div', v))
            }
            return h('div', html)
          }
        },
        {
          title: 'sharding rule',
          key: 'shardingRule',
          render: (h, params) => {
            const rows = params.row.shardingRule.split('\n')
            const html = []
            for (const v of rows) {
              html.push(h('div', v))
            }
            return h('div', html)
          }
        },
        {
          title: 'sql example',
          key: 'sqlExample',
          render: (h, params) => {
            const rows = params.row.sqlExample.split('\n')
            const html = []
            for (const v of rows) {
              html.push(h('div', v))
            }
            return h('div', html)
          }
        }
      ]
    }
  },
  methods: {
    formatData(sourceData) {
      const hash = location.hash
      const showVersion = hash.split('?showVersion=')[1] || ''
      const map = []
      const legend = {}
      const series = {}
      const desc = {}
      const xAxis = {}

      for (const v in sourceData) {
        map.push(v)
        if (v !== 'DESC') {
          legend[v] = []
          series[v] = []
          xAxis[v] = []
        }
      }
      for (const m of map) {
        if (
          Object.prototype.toString.call(sourceData[m]) === '[object Array]'
        ) {
          for (const mm of sourceData[m]) {
            legend[m].push(mm.type)
            const data = []
            for (const mmm of Object.keys(mm.data)) {
              if (xAxis[m].length <= mm.data.length && mmm >= xAxis[m].length) {
                xAxis[m].push(Number(mmm) + 1)
              }
              data.push({
                ...mm.data[mmm],
                value: mm.data[mmm].Throughout,
                Date: moment(mm.data[mmm].Date).format('YYYY-MM-DD HH:mm:ss')
              })
            }

            // showVersion.includes('3.') show 3.X version
            if (showVersion.includes('3.')) {
              series[m].push({
                name: mm.type,
                type: 'line',
                data
              })
            } else {
              if (mm.type.includes('3.')) {
                continue
              }
              series[m].push({
                name: mm.type,
                type: 'line',
                data
              })
            }
          }
        } else {
          for (const k of map) {
            if (k !== m) {
              desc[k] = {
                mysqlVerison: sourceData[m]['mysqlVerison'],
                encryptRule: sourceData[m]['encryptRule'],
                masterSlaveRule: sourceData[m]['masterSlaveRule'],
                shardingRule: sourceData[m]['shardingRule'],
                tableDescription: sourceData[m]['tableDescription'],
                sqlExample: sourceData[m][k].SqlExample
              }
            }
          }
        }
      }
      this.legend = legend
      this.series = series
      this.desc = desc
      this.xAxis = xAxis
    }
  }
}

export { mountedMixin }

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
      modal: false,
      rules: '',
      titleRule: '',
      legend: {},
      series: {},
      desc: {},
      xAxis: {},
      loading: false,
      columns: [
        {
          title: 'MySQL Verison',
          align: 'center',
          key: 'mysqlVerison'
        },
        {
          title: 'Table Description',
          key: 'tableDescription',
          render: (h, params) => {
            if (params.row.tableDescription === 'None') return h('div', '-')
            const rows = params.row.tableDescription.split('\n')
            const html = []
            for (const v of rows) {
              html.push(h('div', v))
            }
            return h('div', html)
          }
        },
        {
          title: 'Encrypt Rule',
          align: 'center',
          key: 'encryptRule',
          render: (h, params) => {
            if (params.row.encryptRule === 'None') return h('div', '-')
            const rows = params.row.encryptRule.split('↵')
            let html = ``
            for (const v of rows) {
              html += v + '\n'
            }
            // const _html = `${JSON.stringify(yaml.safeLoad(html), null, '\t')}`
            return h('div', [
              h(
                'Button',
                {
                  props: {
                    type: 'primary',
                    size: 'small'
                  },
                  on: {
                    click: () => {
                      this.modal = true
                      this.rules = html
                      this.titleRule = 'Encrypt Rule'
                    }
                  }
                },
                'view rules'
              )
            ])
          }
        },
        {
          title: 'Master-slave Rule',
          align: 'center',
          key: 'masterSlaveRule',
          render: (h, params) => {
            if (params.row.masterSlaveRule === 'None') return h('div', '-')
            const rows = params.row.masterSlaveRule.split('↵')
            let html = ``
            for (const v of rows) {
              html += v + '\n'
            }
            // const _html = `${JSON.stringify(yaml.safeLoad(html), null, '\t')}`
            return h('div', [
              h(
                'Button',
                {
                  props: {
                    type: 'primary',
                    size: 'small'
                  },
                  on: {
                    click: () => {
                      this.modal = true
                      this.rules = html
                      this.titleRule = 'Master-slave Rule'
                    }
                  }
                },
                'view rules'
              )
            ])
          }
        },
        {
          title: 'Sharding Rule',
          align: 'center',
          key: 'shardingRule',
          render: (h, params) => {
            if (params.row.shardingRule === 'None') return h('div', '-')
            const rows = params.row.shardingRule.split('↵')
            let html = ``
            for (const v of rows) {
              html += v + '\n'
            }
            // const _html = `${JSON.stringify(yaml.safeLoad(html), null, '\t')}`
            return h('div', [
              h(
                'Button',
                {
                  props: {
                    type: 'primary',
                    size: 'small'
                  },
                  on: {
                    click: () => {
                      this.modal = true
                      this.rules = html
                      this.titleRule = 'Sharding Rule'
                    }
                  }
                },
                'view rules'
              )
            ])
          }
        },
        {
          title: 'SQL Example',
          key: 'sqlExample',
          render: (h, params) => {
            if (params.row.sqlExample === 'None') return h('div', '-')
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
              // data.push({
              //   ...mm.data[mmm],
              //   value: mm.data[mmm].Throughout,
              //   Date: moment(new Date(mm.data[mmm].Date)).format('YYYY-MM-DD HH:mm:ss')
              // })
            }

            const len = mm.data.length - 1
            if (len) {
              for (let i = len; len > 30 ? i >= len - 30 : i >= 0; i--) {
                data.unshift({
                  ...mm.data[i],
                  value: mm.data[i].Throughout,
                  Date: moment(new Date(mm.data[i].Date)).format('YYYY-MM-DD HH:mm:ss')
                })
              }
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

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
          title: 'table number',
          key: 'tableNumber'
        },
        {
          title: 'scene description',
          key: 'sceneDescription'
        },
        {
          title: 'sql example',
          key: 'sqlExample'
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
      const _xAxis = {}
      const _series = {}

      for (const v in sourceData) {
        map.push(v)
        if (v !== 'DESC') {
          legend[v] = []
          series[v] = []
          _series[v] = []
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
              xAxis[m].push(
                moment(mm.data[mmm].Date).format('YYYY-MM-DD HH:mm:ss')
              )
              // if (xAxis[m].length <= mm.data.length && mmm >= xAxis[m].length) {
              //   xAxis[m].push(moment(mm.data[mmm].Date).format('YYYY-MM-DD'))
              // }
              xAxis[m].sort((a, b) => {
                return moment(a) > moment(b) ? 1 : -1
              })
              _xAxis[m] = xAxis[m].filter((el, index, self) => {
                return self.indexOf(el) === index
              })

              data.push({
                ...mm.data[mmm],
                showTip: true,
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

          // Complement logic
          // Complement data at the beginning
          for (const d of series[m]) {
            const _tem = []
            for (const ddd of d.data) {
              _tem.push(ddd.Date)
            }
            for (const dd of _xAxis[m]) {
              if (!_tem.includes(dd)) {
                d.data.unshift({
                  showTip: false,
                  value: null,
                  Date: dd
                })
              }
            }
            d.data.sort((a, b) => {
              return moment(a.Date) > moment(b.Date) ? 1 : -1
            })
          }
        } else {
          for (const k of map) {
            if (k !== m) {
              desc[k] = {
                mysqlVerison: sourceData[m]['mysqlVerison'],
                tableNumber: sourceData[m]['tableNumber'],
                sceneDescription: sourceData[m]['sceneDescription'],
                sqlExample: sourceData[m][k].SqlExample
              }
            }
          }
        }
      }
      this.legend = legend
      this.series = series
      this.desc = desc
      this.xAxis = _xAxis
    }
  }
}

export { mountedMixin }

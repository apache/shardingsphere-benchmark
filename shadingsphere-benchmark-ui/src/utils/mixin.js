const mountedMixin = {
  data() {
    return {
      legend: {},
      series: {},
      desc: {},
      xAxis: {},
      loading: false
    }
  },
  methods: {
    formatData(sourceData) {
      const hash = location.hash
      const showType = hash.split('?showType=')[1]
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
              if (xAxis[m].length !== mm.data.length) {
                xAxis[m].push(mmm)
              }
              data.push({
                ...mm.data[mmm],
                value: mm.data[mmm].Throughout
              })
            }

            // showType === 3.0 show 3.0version
            if (showType === '3.0') {
              series[m].push({
                name: mm.type,
                type: 'line',
                data
              })
            } else {
              if (mm.type.includes('3.0')) {
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
      this.xAxis = xAxis
    }
  }
}

export { mountedMixin }

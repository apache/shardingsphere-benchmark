const mountedMixin = {
  data() {
    return {
      legend: {},
      series: {},
      desc: {},
      xAxis: {}
    }
  },
  mounted() {
    const map = []
    const legend = {}
    const series = {}
    const desc = {}
    const xAxis = {}
    for (const v in this.data) {
      map.push(v)
      if (v !== 'DESC') {
        legend[v] = []
        series[v] = []
        xAxis[v] = []
      }
    }
    for (const m of map) {
      if (Object.prototype.toString.call(this.data[m]) === '[object Array]') {
        for (const mm of this.data[m]) {
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
          series[m].push({
            name: mm.type,
            type: 'line',
            data
          })
        }
      } else {
        for (const k of map) {
          if (k !== m) {
            desc[k] = {
              mysqlVerison: this.data[m]['mysqlVerison'],
              // 'ShardingSphere-proxy': this.data[m]['ShardingSphere-proxy'],
              tableNumber: this.data[m]['tableNumber'],
              // DataVolume: this.data[m]['DataVolume'],
              // X: this.data[m]['X'],
              // Y: this.data[m]['Y'],
              sceneDescription: this.data[m]['sceneDescription'],
              sqlExample: this.data[m][k].SqlExample
              // ComparativeType: this.data[m][k].ComparativeType
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

export { mountedMixin }

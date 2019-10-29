const color = ['#2D8CF0', '#9A66E4', '#19BE6B', '#FF9900', '#E46CBB']
const getLineOptions = (name, xAxis, legend, series) => {
  let _xAxis = xAxis[name]
  const len = xAxis[name].length
  if (len > 30) {
    _xAxis = xAxis[name].slice(len - 30, len)
  }
  return {
    color,
    grid: {
      left: '2%',
      right: '2%',
      bottom: '6%',
      containLabel: true
    },
    tooltip: {
      trigger: 'axis',
      formatter(d) {
        let html = ``
        for (const v of d) {
          let p = ``
          for (const vv in v.data) {
            if (vv !== 'value' && vv !== 'Date') {
              p += `<p>${vv}: ${v.data[vv]}</p>`
            }
          }
          html += `<div style="display:inline-block;margin: 10px;font-size: 14px;">
            <p><span style="display:inline-block;margin-right:5px;border-radius:10px;width:10px;height:10px;background-color:${
              v.color
            };"></span>${v.data.Date}</p> 
            <p>${v.seriesName}</p>
            ${p}
          </div>`
        }
        return html ? `${html}` : ''
      },
      position(pos, params, dom, rect, size) {
        if (size.viewSize[0] > 300 && size.viewSize[0] < 510) {
          const obj = { top: 60 }
          obj[['left', 'right'][+(pos[0] < size.viewSize[0] / 2)]] = 5
          return obj
        }
      }
    },
    xAxis: {
      name: 'Construction times',
      nameLocation: 'middle',
      type: 'category',
      boundaryGap: false,
      data: _xAxis
    },
    yAxis: {
      name: 'TPS',
      type: 'value'
    },
    legend: {
      data: legend[name],
      right: '2%'
    },
    series: series[name]
  }
}

export { getLineOptions }

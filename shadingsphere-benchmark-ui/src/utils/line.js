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
          if (v.data.value !== 'null') {
            for (const vv in v.data) {
              if (vv !== 'value' && vv !== 'Date') {
                p += `<p>${vv}: ${v.data[vv]}</p>`
              }
            }
          }
          if (v.data.Date !== 'Invalid date') {
            html += `<div style="display:inline-block;margin: 10px;font-size: 14px;">
            <p><span style="display:inline-block;margin-right:5px;border-radius:10px;width:10px;height:10px;background-color:${v.color};"></span>${v.data.Date}</p> 
            <p>${v.seriesName}</p>
            ${p}
          </div>`
          }
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
      nameGap: 20,
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

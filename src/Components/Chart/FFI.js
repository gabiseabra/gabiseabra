'use-strict'

const {Chart} = require('chart.js')

const rgba = (rgb, a) => `rgb(${rgb.join(',')},${a})`

const parseData = {
  radar: ({labels, datasets}) => ({
    labels,
    datasets: datasets.map(({label, color, data}) => ({
      label,
      data,
      backgroundColor: rgba(color, 0.2),
      borderColor: rgba(color, 1),
      borderWidth: 1,
      pointBackgroundColor: 'transparent',
      pointHoverBackgroundColor: 'transparent',
      pointBorderColor: 'transparent',
      pointHoverBorderColor: 'transparent'
    }))
  })
}

exports.create_ = ({type, data}) => (element) => () =>
  new Chart(element.getContext('2d'), {type, data: parseData[type](data)})

exports.destroy = (chart) => () => chart.destroy()

exports.reset = (chart) => () => chart.reset()

exports.update_ = (duration) => (easing) => (chart) => () =>
  chart.update({duration, easing})

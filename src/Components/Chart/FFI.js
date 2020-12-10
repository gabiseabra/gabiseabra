"use-strict"

const {Chart} = require('chart.js')

exports.create_ = (opts) => (element) => () =>
  new Chart(element.getContext('2d'), opts)

exports.destroy = (chart) => () => chart.destroy()

exports.reset = (chart) => () => chart.reset()

exports.update_ = (duration) => (easing) => (chart) => () =>
  chart.update({ duration, easing })

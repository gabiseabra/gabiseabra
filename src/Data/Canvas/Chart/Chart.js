const {mkCanvas2D, watchSize} = require('../../../lib/2d/canvas')
const radarChart = require('../../../lib/2d/objects/radarChart')

const getSize = () => {
  const size = Math.min(window.innerWidth - 30, window.innerHeight * 0.45)
  return {width: size, height: size}
}

exports.mkCanvas = (options) => () => {
  const canvas = mkCanvas2D('chart', getSize())

  const draw = () => {
    radarChart.draw(canvas, options)
  }

  watchSize(canvas, getSize, draw)

  return canvas
}

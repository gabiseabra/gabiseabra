const {mkCanvas2D, watchSize} = require('../../../lib/2d/canvas')
const Chart = require('../../../lib/2d/objects/Chart')

const getSize = () => {
  const size = Math.min(window.innerWidth - 30, window.innerHeight * 0.5)
  return {width: size, height: size}
}

const stage = ({app, width}, {datasets, labels}) => {
  app.stage.addChild(
    new Chart.Scale({
      center: {x: width / 2, y: width / 2},
      labels: labels,
      sections: 3,
      radius: width / 2 - 60
    })
  )
}

exports.mkCanvas = (options) => () => {
  const canvas = mkCanvas2D('chart', getSize())

  stage(canvas, options)

  watchSize(canvas, getSize)

  return canvas
}

const {mkCanvas2D, watchSize} = require('../../../lib/2d/canvas')
const Chart = require('../../../lib/2d/objects/Chart')

const getSize = () => {
  const size = Math.min(window.innerWidth - 30, window.innerHeight * 0.5)
  return {width: size, height: size}
}

const draw = ({datasets, labels}) => (canvas) => {
  // const max = Math.max(...[].concat(...datasets.map((d) => d.data)))
  const center = {x: canvas.width / 2, y: canvas.width / 2}
  const radius = canvas.width / 2 - 60

  Chart.mkScale(canvas.ctx, {
    center,
    radius,
    labels: labels,
    sections: 3
  })

  /*
  datasets.forEach(({data}) =>
    app.stage.addChild(
      new Chart.Dataset(app, {
        center,
        radius,
        max,
        data
      })
    )
  )
  */
}

exports.mkCanvas = (options) => () => {
  const canvas = mkCanvas2D('chart', draw(options), getSize())

  watchSize(canvas, getSize)

  return canvas
}

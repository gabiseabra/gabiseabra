const {mkCanvas2D, watchSize} = require('../../../lib/2d/canvas')
const Chart = require('../../../lib/2d/objects/chart')

const getSize = () => {
  const size = Math.min(window.innerWidth - 50, window.innerHeight * 0.5)
  return {width: size, height: size}
}

const draw = ({datasets, labels}) => ({width, height, ctx}) => {
  const max = Math.max(...[].concat(...datasets.map((d) => d.data)))
  const center = {x: width / 2, y: width / 2}
  const radius = width / 2 - 60

  ctx.clearRect(0, 0, width, height)

  Chart.mkScale(ctx, {
    center,
    radius,
    labels: labels,
    sections: 3
  })

  datasets.forEach(({data}) =>
    Chart.mkDataset(ctx, {
      center,
      width,
      height,
      radius,
      max,
      data
    })
  )
}

exports.mkCanvas = (options) => () => {
  const canvas = mkCanvas2D('chart', draw(options), getSize())

  watchSize(canvas, getSize)

  return canvas
}

// pink
const GRADIENT_COLOR = ['#ff587d90', '#ffcaca70']
const BORDER_COLOR = '#ff587d'
// purple
// const GRADIENT_COLOR = ['#8f97ef70', '#c9ceff70']
// const BORDER_COLOR = '#8f97ef'

function traceDataset(ctx, {data, max, radius, center}) {
  const edges = data.length

  ctx.beginPath()

  data.forEach((n, idx) => {
    const a = (idx / edges) * Math.PI * 2
    const x = center.x + Math.cos(a) * radius * (n / max) * 0.97
    const y = center.y - Math.sin(a) * radius * (n / max) * 0.97
    if (idx == 0) ctx.moveTo(x, y)
    else ctx.lineTo(x, y)
  })

  ctx.closePath()
}

/*
const getPos = ({radius, angle, center}) => {
  const x = center.x + Math.cos(angle) * radius
  const y = center.y - Math.sin(angle) * radius
  return {x, y}
}

function linearGradient(ctx, {radius, center}) {
  const pos0 = getPos({radius, center, angle: Math.PI / 2})
  const pos1 = getPos({radius, center, angle: Math.PI * 3})

  const bg = ctx.createLinearGradient(pos0.x, pos0.y, pos1.x, pos1.y)
  bg.addColorStop(0.3, '#c9ceff80')
  bg.addColorStop(0.7, '#8f97ef90')

  return bg
}
*/

function radialGradient(ctx, {radius, center}) {
  const bg = ctx.createRadialGradient(
    center.x,
    center.y,
    radius,
    center.x,
    center.y,
    radius / 4
  )
  bg.addColorStop(0, GRADIENT_COLOR[0])
  bg.addColorStop(1, GRADIENT_COLOR[1])
  return bg
}

function drawDataset(ctx, opts) {
  ctx.fillStyle = radialGradient(ctx, opts)
  ctx.fill()
  ctx.lineCap = 'round'
  ctx.strokeStyle = BORDER_COLOR
  ctx.strokeWidth = 2
  ctx.stroke()
}

export const mkDataset = (ctx, opts) => {
  traceDataset(ctx, opts)
  drawDataset(ctx, opts)
}

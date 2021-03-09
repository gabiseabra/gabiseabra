const getPos = (i, {radius, edges, center}) => {
  const a = (i / edges) * Math.PI * 2
  const x = center.x + Math.cos(a) * radius
  const y = center.y - Math.sin(a) * radius
  return {angle: a, x, y}
}

function outlineSection(ctx, {radius, edges, center}) {
  ctx.beginPath()

  ctx.moveTo(center.x, center.y - radius)

  for (let i = 1; i <= edges + 1; i++) {
    const {x, y} = getPos(i, {radius, edges, center})
    ctx.lineTo(x, y)
  }

  ctx.closePath()

  return ctx
}

function outlineLines(ctx, {radius, edges, center}) {
  ctx.beginPath()

  for (let i = 0; i < edges; ++i) {
    const {x, y} = getPos(i, {radius, edges, center})
    ctx.moveTo(x, y)
    ctx.lineTo(center.x, center.y)
  }
}

function drawBackground(ctx) {
  ctx.fillStyle = '#72d6e7'
  ctx.strokeStyle = '#72d6e7'
  ctx.strokeWidth = 40
  ctx.fill()
  ctx.stroke()
}

function drawLine(ctx) {
  ctx.strokeStyle = '#d3efff'
  ctx.strokeWidth = 0.8
  ctx.stroke()
}

function drawLabel(ctx, text, x, y) {
  const fontSize = 14
  ctx.font = `600 ${fontSize}px "Fira Mono"`
  const {width} = ctx.measureText(text)
  const height = fontSize
  const center = {
    x: x - width / 2,
    y: y - height / 2
  }
  ctx.fillStyle = '#72d6e7'
  ctx.fillRect(center.x - 5, center.y + 3, width * 1.2, height)
  ctx.fillStyle = '#005498'
  ctx.fillText(text, center.x - 5, center.y + 10)
}

export const mkScale = (ctx, {center, labels, sections, radius}) => {
  const edges = labels.length
  const opts = {center, radius, edges}

  // background
  outlineSection(ctx, {...opts, radius: radius * 1.05})
  drawBackground(ctx)
  // section lines
  for (let i = 0; i < sections; ++i) {
    const n = (i + 1) / sections
    outlineSection(ctx, {...opts, radius: radius * n})
    drawLine(ctx)
  }
  outlineLines(ctx, opts)
  drawLine(ctx)
  // labels
  labels.forEach((text, idx) => {
    const {x, y} = getPos(idx, {...opts, radius: radius * 1.2})
    drawLabel(ctx, text, x, y)
  })
}

export function draw({ctx, width, height}, {labels, datasets}) {
  const center = {x: width / 2, y: height / 2}

  console.log(labels)
  const step = 5
  const max =
    Math.ceil(
      Math.max(...[].concat(...datasets.map(({data}) => data))) / step
    ) * step
  const coords = drawScale(ctx, {
    center,
    edges: datasets[0].data.length,
    sections: 4,
    radius: height / 2 - 60
  })
  drawLabels(ctx, {
    coords,
    labels
  })
}

function drawScale(ctx, {center, edges, sections, radius}) {
  const coords = []

  for (let i = 0; i < sections; i++) {
    const r = (radius / sections) * (i + 1)

    ctx.beginPath()
    ctx.moveTo(center.x, center.y - r)

    for (let j = 1; j <= edges + 1; j++) {
      const a = (j / edges) * Math.PI * 2
      const xy = {
        x: center.x + Math.cos(a) * r,
        y: center.y - Math.sin(a) * r
      }
      ctx.lineTo(xy.x, xy.y)
      if (i == sections - 1)
        coords.push({
          ...xy,
          angle: a,
          radius: r
        })
    }

    ctx.closePath()
    styleScale(ctx)
  }
  ctx.globalCompositeOperation = 'destination-over'
  ctx.fillStyle = '#ffcaca'
  ctx.fill()

  coords.forEach((coord) => {
    ctx.moveTo(center.x, center.y)
    ctx.lineTo(coord.x, coord.y)
  })

  coords.pop()

  styleScale(ctx)

  return coords
}

function styleScale(ctx) {
  ctx.globalCompositeOperation = 'destination-over'
  ctx.lineWidth = 16
  ctx.strokeStyle = '#ffcaca'
  ctx.stroke()
  ctx.globalCompositeOperation = 'source-over'
  ctx.lineWidth = 0.8
  ctx.strokeStyle = '#ffa1b4'
  ctx.stroke()
}

function drawLabels(ctx, {coords, labels}) {
  ctx.font = '12px "Fira Mono"'
  ctx.textAlign = 'center'
  ctx.fillStyle = '#f82257'
  ctx.strokeStyle = 'white'
  ctx.lineWidth = 3
  ctx.lineCap = 'round'

  coords.forEach(({x, y, angle}, idx) => {
    const xy = {
      x: x + Math.cos(angle) * 30,
      y: y - Math.sin(angle) * 30
    }

    const label = labels[idx]
    ctx.strokeText(label, xy.x, xy.y)
    ctx.fillText(label, xy.x, xy.y)
  })
}

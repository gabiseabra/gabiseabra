import * as PIXI from 'pixi.js'

function outlineSection(g, {radius, edges, center}) {
  g.moveTo(center.x, center.y - radius)

  for (let j = 1; j <= edges + 1; j++) {
    const a = (j / edges) * Math.PI * 2
    const xy = {
      x: center.x + Math.cos(a) * radius,
      y: center.y - Math.sin(a) * radius
    }
    g.lineTo(xy.x, xy.y)
  }

  return g
}

export class Scale extends PIXI.Graphics {
  static FILL_COLOR = 0x72d6e7
  static HIGHLIGHT_COLOR = 0x72d6e7

  static LINE_STYLE = {
    width: 0.8,
    color: 0xd3efff
  }

  static LABEL_STYLE = new PIXI.TextStyle({
    fontFamily: 'Fira Mono',
    fontWeight: '600',
    fontSize: 14,
    fill: 0x005498
  })

  constructor({center, labels, sections, radius}) {
    super()

    const edges = labels.length
    const getAngle = (i) => (i / edges) * Math.PI * 2
    const getCoords = (i, n = 1) => {
      const a = getAngle(i)
      return {
        x: center.x + Math.cos(a) * (radius * n),
        y: center.y - Math.sin(a) * (radius * n)
      }
    }

    // scale fill
    {
      const fill = new PIXI.Graphics()
      fill.beginFill(Scale.FILL_COLOR)
      outlineSection(fill, {
        center,
        edges,
        radius: radius * 1.05
      })
      fill.closePath()
      fill.lineStyle(Scale.LINE_STYLE)
      for (let i = 0; i < edges; ++i) {
        const {x, y} = getCoords(i)
        fill.moveTo(x, y)
        fill.lineTo(center.x, center.y)
      }
      this.addChild(fill)
    }
    // section lines
    {
      for (let i = 0; i < sections; i++) {
        const n = (i + 1) / sections

        const section = new PIXI.Graphics()
        section.beginFill(0, 0)
        section.lineStyle(Scale.LINE_STYLE)
        outlineSection(section, {
          center,
          edges,
          radius: radius * n
        })
        section.closePath()
        this.addChild(section)
      }
    }
    // labels
    {
      labels.forEach((text, idx) => {
        const {x, y} = getCoords(idx, 1.2)
        const label = new PIXI.Text(text, Scale.LABEL_STYLE)
        label.resolution = window.devicePixelRatio || 1
        label.x = x - label.width / 2
        label.y = y - label.height / 2
        const highlight = new PIXI.Graphics()
        highlight.beginFill(Scale.HIGHLIGHT_COLOR)
        highlight.drawRect(
          label.x - 2,
          label.y + 2,
          label.width + 4,
          label.height * 0.8
        )
        this.addChild(highlight)
        this.addChild(label)
      })
    }
  }
}

export const pixelRatio = (ctx) => {
  const dpr = window.devicePixelRatio || 1
  const bsr =
    ctx.webkitBackingStorePixelRatio ||
    ctx.mozBackingStorePixelRatio ||
    ctx.msBackingStorePixelRatio ||
    ctx.oBackingStorePixelRatio ||
    ctx.backingStorePixelRatio ||
    1

  return dpr / bsr
}

export const mkCanvas2D = (id, draw, {width, height}) => {
  const element = document.createElement('canvas')
  const ctx = element.getContext('2d')
  const resolution = pixelRatio(ctx)

  element.width = width * resolution
  element.height = height * resolution
  element.style.width = `${width}px`
  element.style.height = `${height}px`
  element.id = id

  ctx.setTransform(resolution, 0, 0, resolution, 0, 0)

  const canvas = {
    element,
    ctx,
    width,
    height,
    resolution,
    listeners: [],
    draw: () => draw(canvas),
    destroy: () => destroy(canvas)
  }

  canvas.draw()

  return canvas
}

export const watchSize = (canvas, getSize) => {
  const listener = () => setSize(canvas, getSize())
  window.addEventListener('resize', listener)
  canvas.listeners.push(() => window.addEventListener('resize', listener))
}

export const setSize = (canvas, {width, height}) => {
  const {element, resolution} = canvas

  element.width = canvas.width = width * resolution
  element.height = canvas.height = height * resolution
  element.style.width = width
  element.style.height = height

  canvas.draw()
}

export const destroy = (canvas) => {
  canvas.listeners.forEach((fn) => fn())
  canvas.listeners = []
}

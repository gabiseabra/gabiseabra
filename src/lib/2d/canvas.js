export const mkCanvas2D = (id, {width, height}) => {
  const element = document.createElement('canvas')
  element.id = id

  const context = element.getContext('2d')

  const canvas = {
    ctx: context,
    width,
    height,
    element,
    listeners: []
  }

  setSize(canvas, {width, height})

  return canvas
}

export const addResizeListener = (canvas, getSize) => {
  const listener = () => setSize(canvas, getSize())
  window.addEventListener('resize', listener)
  canvas.listeners.push(() => window.addEventListener('resize', listener))
}

export const setSize = (canvas, {width, height}) => {
  canvas.element.width = width
  canvas.element.height = height
  canvas.width = width
  canvas.height = height
}

export const watchSize = (canvas, getSize, draw) => {
  addResizeListener(canvas, getSize)
  setSize(canvas, getSize())
  draw(canvas)
}

export const destroy = (canvas) => {
  canvas.listeners.forEach((fn) => fn())
  canvas.element.remove()
  canvas.listeners = []
}

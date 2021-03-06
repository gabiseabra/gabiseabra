const PIXI = require('pixi.js')

export const mkCanvas2D = (id, {width, height}) => {
  const app = new PIXI.Application({
    antialias: true,
    backgroundAlpha: 0
  })
  const element = app.view
  element.id = id

  const canvas = {
    app,
    element,
    width,
    height,
    aspect: width / height,
    listeners: [],
    destroy: () => destroy(canvas)
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
  const {aspect, element} = canvas
  let w, h
  if (width / height >= aspect) {
    w = width * aspect
    h = height
  } else {
    w = width
    h = height / aspect
  }

  element.width = canvas.width = w
  element.height = canvas.height = h
}

export const watchSize = (canvas, getSize) => {
  addResizeListener(canvas, getSize)
  setSize(canvas, getSize())
}

export const destroy = (canvas) => {
  canvas.listeners.forEach((fn) => fn())
  canvas.app.destroy(true)
  canvas.listeners = []
}

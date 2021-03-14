let stats = {begin() {}, end() {}}

if (process.env.NODE_ENV == 'development') {
  const Stats = require('stats.js')
  stats = new Stats()
  document.body.appendChild(stats.dom)
}

export const mkCanvas3D = (id, {camera, renderer, scene, composer}) => {
  const element = renderer.domElement
  element.id = id

  const canvas = {
    camera,
    renderer,
    composer,
    scene,
    element,
    listeners: [],
    render: () => render(canvas),
    destroy: () => destroy(canvas)
  }

  return canvas
}

export const setSize = (
  {camera, renderer, composer},
  {width, height, fov, aspect = width / height}
) => {
  if (!isNaN(aspect)) camera.aspect = aspect
  if (!isNaN(fov)) camera.fov = fov
  camera.updateProjectionMatrix()

  renderer.setSize(width, height)

  if (composer) composer.setSize(width, height)
}

export const watchSize = (canvas, getSize, fn) => {
  setSize(canvas, getSize())
  const listener = () => {
    setSize(canvas, getSize())
    if (fn) fn()
  }
  window.addEventListener('resize', listener)
  canvas.listeners.push(() => window.addEventListener('resize', listener))
}

export const render = (canvas) => {
  if (canvas.composer) canvas.composer.render()
  else canvas.renderer.render(canvas.scene, canvas.camera)
}

export const isAnimated = (canvas) => Boolean(canvas.animationFrame)

export const animate = (canvas, fn) => {
  if (isAnimated(canvas)) return

  function go() {
    stats.begin()
    if (fn) fn()
    if (canvas.scene.animating) render(canvas)
    stats.end()
    canvas.animationFrame = requestAnimationFrame(go)
  }

  go()
}

export const pause = (canvas) => {
  if (!isAnimated(canvas)) return
  cancelAnimationFrame(canvas.animationFrame)
  canvas.animationFrame = null
}

export const destroy = (canvas) => {
  const {scene} = canvas

  pause(canvas)
  canvas.listeners.forEach((fn) => fn())

  while (scene.children.length > 0) {
    scene.remove(scene.children[0])
  }

  canvas.renderer = null
  canvas.composer = null
  canvas.scene = null
  canvas.camera = null
  canvas.listeners = []
}

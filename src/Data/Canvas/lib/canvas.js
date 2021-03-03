export const mkCanvas = (id, {camera, renderer, scene, composer}) => {
  const element = renderer.domElement
  element.id = id

  return {
    camera,
    renderer,
    composer,
    scene,
    element,
    listeners: []
  }
}

export const addResizeListener = (canvas, getSize) => {
  const listener = () => setSize(canvas, getSize())
  window.addEventListener('resize', listener)
  canvas.listeners.push(() => window.addEventListener('resize', listener))
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

export const watchSize = (canvas, getSize) => {
  addResizeListener(canvas, getSize)
  setSize(canvas, getSize())
}

export const animate = (canvas) => {
  function go() {
    if (canvas.composer) canvas.composer.render()
    else canvas.renderer.render(canvas.scene, canvas.camera)
    canvas.animationFrame = requestAnimationFrame(go)
  }

  go()
}

export const pause = (canvas) => {
  cancelAnimationFrame(canvas.animationFrame)
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

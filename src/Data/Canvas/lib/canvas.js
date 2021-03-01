import * as THREE from 'three'

export const addResizeListener = (canvas, getSize) => {
  canvas.resizeListener = () => {
    const { width, height } = getSize()
    setSize(canvas, width, height)
  }
  window.addEventListener('resize', canvas.resizeListener)
}

export const removeResizeListener = (canvas) => {
  if (canvas.resizeListener) {
    window.removeEventListener('resize', canvas.resizeListener)
  }
  delete canvas.resizeListener
}

export const setSize = ({ camera, renderer, composer }, width, height) => {
  renderer.setSize(width, height)
  if (camera instanceof THREE.PerspectiveCamera) {
    camera.aspect = width / height
  } else if (camera instanceof THREE.OrthographicCamera) {
    camera.top = camera.bottom = height / 2
    camera.left = camera.right = width / 2
  }
  camera.updateProjectionMatrix()
  if (composer) this.composer.setSize(width, height)
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
  const { scene } = canvas

  pause(canvas)
  removeResizeListener(canvas)

  while(scene.children.length > 0){
    scene.remove(scene.children[0]);
  }

  canvas.renderer = null
  canvas.composer = null
  canvas.scene = null
  canvas.camera = null
}

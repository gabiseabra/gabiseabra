import * as THREE from 'three'

export const watchSize = (canvas, getSize) => {
  window.addEventListener('resize', () => {
    const { width, height } = getSize()
    setSize(canvas, width, height)
  })
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

export const animate = ({ renderer, composer, scene, camera }) => {
  console.log(renderer, composer, scene, camera)
  function go() {
    if (composer) composer.render()
    else renderer.render(scene, camera)
    requestAnimationFrame(go)
  }

  go()
}

const THREE = require('three')
const config = require('three-playground/src/theme/config').default
const { watchSize, setSize, animate } = require('../lib/canvas')
const { Scene } = require('./Scene')

const getSize = () => ({
  width: window.innerWidth,
  height: window.innerHeight
})

exports.mkCanvas = () => {
  const { width, height } = getSize()

  const renderer = new THREE.WebGLRenderer({
    powerPreference: 'high-performance',
    antialias: true
  })
  const element = renderer.domElement
  element.id = 'background-scene'

  const camera = new THREE.PerspectiveCamera(
      config.camera.fov,
      width / height,
      1,
      config.camera.far
    )

  const scene = new Scene({ camera, config })

  const canvas = { renderer, camera, scene, element }

  watchSize(canvas, getSize)
  setSize(canvas, width, height)

  animate(canvas)

  window.Scroller.addEventListener(({progress}) => {
    scene.progress = progress
  })

  return canvas
}

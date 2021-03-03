const THREE = require('three')
const config = require('three-playground/src/theme/config').default
const {animate, mkCanvas, watchSize} = require('../lib/canvas')
const {Scene} = require('./Scene')

const getSize = () => ({
  width: window.innerWidth,
  height: window.innerHeight
})

exports.mkCanvas = () => {
  const {width, height} = getSize()

  const renderer = new THREE.WebGLRenderer({
    powerPreference: 'high-performance',
    antialias: true
  })

  const camera = new THREE.PerspectiveCamera(
    config.camera.fov,
    width / height,
    1,
    config.camera.far
  )

  const scene = new Scene({camera, config})

  const canvas = mkCanvas('background-scene', {renderer, camera, scene})

  watchSize(canvas, getSize)

  animate(canvas)

  window.Scroller.addEventListener(({progress}) => {
    scene.progress = progress
  })

  return canvas
}

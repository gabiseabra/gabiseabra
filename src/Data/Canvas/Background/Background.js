const THREE = require('three')
const {
  animate,
  mkCanvas3D,
  watchSize,
  render,
  pause
} = require('../../../lib/3d/canvas')
const {Effects} = require('../../../lib/3d/postprocessing/Effects')
const {Scene} = require('./Scene')
const config = require('./config')

const NARROW_FOV = 90
const WIDE_FOV = 45

const getSize = () => {
  const width = window.innerWidth
  const height = window.innerHeight
  const aspect = width / height
  const fov = THREE.MathUtils.lerp(
    NARROW_FOV,
    WIDE_FOV,
    Math.min(width, 760) / 760
  )
  return {width, height, aspect, fov}
}

exports.mkCanvas = () => {
  const {width, height, aspect, fov} = getSize()

  const loader = new THREE.LoadingManager()

  const renderer = new THREE.WebGLRenderer({
    powerPreference: 'high-performance',
    antialias: false
  })
  renderer.setPixelRatio(window.devicePixelRatio)

  const camera = new THREE.PerspectiveCamera(fov, aspect, 1, config.camera.far)

  const scene = new Scene({camera, config, loader})

  const composer = new Effects({scene, camera, renderer, width, height})

  const canvas = mkCanvas3D('background-scene', {
    renderer,
    camera,
    scene,
    composer
  })

  render(canvas)

  watchSize(canvas, getSize)

  return canvas
}

exports.setScroller = (canvas) => (scroller) => () => {
  pause(canvas)

  let prevProgress = null

  animate(canvas, () => {
    const progress = scroller.progress

    if (!isNaN(progress) && progress != prevProgress) {
      prevProgress = progress
      canvas.scene.progress = progress
      canvas.composer.progress = progress
    }
  })
}

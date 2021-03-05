const THREE = require('three')
const config = require('three-playground/src/theme/config').default
const {animate, mkCanvas, watchSize} = require('../../../lib/3d/canvas')
const {Scene} = require('./Scene')

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
  const {aspect, fov} = getSize()

  const renderer = new THREE.WebGLRenderer({
    powerPreference: 'high-performance',
    antialias: true
  })

  const camera = new THREE.PerspectiveCamera(fov, aspect, 1, config.camera.far)

  const scene = new Scene({camera, config})

  const canvas = mkCanvas('background-scene', {renderer, camera, scene})

  watchSize(canvas, getSize)

  animate(canvas)

  window.Scroller.addEventListener(({progress}) => {
    scene.progress = progress
  })

  return canvas
}

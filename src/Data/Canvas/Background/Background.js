const THREE = require('three')
const {animate, mkCanvas3D, watchSize} = require('../../../lib/3d/canvas')
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
  const {aspect, fov} = getSize()

  const renderer = new THREE.WebGLRenderer({
    powerPreference: 'high-performance',
    antialias: true
  })

  const camera = new THREE.PerspectiveCamera(fov, aspect, 1, config.camera.far)

  const scene = new Scene({camera, config})

  const canvas = mkCanvas3D('background-scene', {renderer, camera, scene})

  watchSize(canvas, getSize)

  animate(canvas)

  window.Scroller.addEventListener(({progress}) => {
    scene.progress = progress
  })

  return canvas
}

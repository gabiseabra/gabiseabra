const THREE = require('three')
const config = require('three-playground/src/theme/config').default
const {animate, mkCanvas3D, watchSize} = require('../../../lib/3d/canvas')
const {Effects} = require('../../../lib/3d/postprocessing/Effects')
const {Scene} = require('./Scene')
const {addObject} = require('three-playground/src/lib/GUI/properties')
const Dat = require('dat.gui')

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

  watchSize(canvas, getSize)

  animate(canvas)

  const gui = new Dat.GUI()
  addObject(gui, 'post processing', composer)

  window.Scroller.addEventListener(({progress}) => {
    scene.progress = progress
    composer.progress = progress
  })

  return canvas
}

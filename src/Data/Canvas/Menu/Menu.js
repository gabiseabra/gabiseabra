const THREE = require('three')
const {animate, mkCanvas, watchSize} = require('../lib/canvas')
const {addOrbitControl} = require('../lib/orbit')
const {Scene} = require('./Scene')

const MAX_WIDTH = 500
const NEAR = 1
const FAR = 1000
const H_FOV = Math.PI / 4

const getSize = () => {
  const width = Math.min(window.innerWidth - 30, MAX_WIDTH)
  const height = 80
  const aspect = width / height
  const vFov = 2 * Math.atan(Math.tan(H_FOV / 2) / aspect)
  return {width, height, aspect, fov: THREE.MathUtils.radToDeg(vFov)}
}

exports.mkCanvas = (links) => () => {
  const {aspect, fov} = getSize()

  const renderer = new THREE.WebGLRenderer({
    powerPreference: 'high-performance',
    antialias: true,
    alpha: true
  })

  const camera = new THREE.PerspectiveCamera(fov, aspect, NEAR, FAR)
  camera.position.set(0, 20, 220)
  camera.lookAt(0, 0, 0)

  const scene = new Scene(links)

  const canvas = mkCanvas({renderer, camera, scene})
  canvas.element.id = 'menu-scene'

  watchSize(canvas, getSize)
  addOrbitControl(canvas, {
    azimuthAngle: THREE.MathUtils.degToRad(2),
    polarAngle: THREE.MathUtils.degToRad(4)
  })

  animate(canvas)

  return canvas
}

exports.setActive = ({scene}) => (id) => scene.setActive(id)

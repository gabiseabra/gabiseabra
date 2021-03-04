const THREE = require('three')
const {animate, mkCanvas, watchSize} = require('../lib/canvas')
const {mkOrbitControl} = require('../lib/orbit')
const {mkRayCaster} = require('../lib/raycaster')
const {Scene} = require('./Scene')

const MAX_WIDTH = 500
const NEAR = 1
const FAR = 1000
const H_FOV = Math.PI / 4

const getSize = () => {
  const width = Math.min(window.innerWidth - 30, MAX_WIDTH)
  const height = 60
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

  const canvas = mkCanvas('menu-scene', {renderer, camera, scene})

  watchSize(canvas, getSize)
  mkOrbitControl(canvas, {
    azimuthAngle: THREE.MathUtils.degToRad(2),
    polarAngle: THREE.MathUtils.degToRad(4)
  })
  const rayCaster = mkRayCaster(canvas)

  canvas.element.addEventListener('mousedown', () => {
    const [intersection] = rayCaster.intersectObjects(scene.nav.children, false)
    if (!intersection || !links[intersection.object.linkIndex]) return
    const link = links[intersection.object.linkIndex]
    link.onClick()
  })

  animate(canvas)

  return canvas
}

exports.setActive = ({scene}) => (id) => scene.nav.setActive(id)

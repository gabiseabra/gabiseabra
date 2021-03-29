const THREE = require('three')
const {
  mkCanvas3D,
  watchSize,
  animate,
  pause
} = require('../../../lib/3d/canvas')
const {mkOrbitControl} = require('../../../lib/3d/orbit')
const {mkRayCaster} = require('../../../lib/3d/raycaster')
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

  const canvas = mkCanvas3D('menu-scene', {renderer, camera, scene})

  const onUpdate = () => {
    scene.needsUpdate = true
  }

  watchSize(canvas, getSize, onUpdate)

  mkOrbitControl(canvas, {
    azimuthAngle: THREE.MathUtils.degToRad(4),
    polarAngle: THREE.MathUtils.degToRad(4),
    onChange: onUpdate
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

exports.setScroller = (canvas) => (scroller) => () => {
  let prevProgress

  pause(canvas)

  animate(canvas, () => {
    const progress = scroller.progress
    if (!isNaN(progress) && progress !== prevProgress) {
      canvas.scene.progress = progress
    }
  })

  canvas.scene.setActive(scroller.triggers.current)

  scroller.addEventListener('scrolltrigger', ({id}) => {
    canvas.scene.setActive(id)
  })
}

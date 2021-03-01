const THREE = require('three')
const { OrbitControls } = require('three/examples/jsm/controls/OrbitControls')
const { addResizeListener, setSize, animate } = require('../lib/canvas')
const { Scene } = require('./Scene')

const MAX_WIDTH = 500
const NEAR = 1
const FAR = 1000
const H_FOV = Math.PI / 4

const DEG = (180 / Math.PI)

const getSize = () => ({
  width: Math.min(window.innerWidth - 30, MAX_WIDTH),
  height: 80
})

const getFov = (aspect) => 2 * Math.atan(Math.tan(H_FOV / 2) / aspect)

exports.mkCanvas = (links) => () => {
  const { width, height } = getSize()
  const aspect = width / height
  const vFov = getFov(aspect) * DEG

  const renderer = new THREE.WebGLRenderer({
    powerPreference: 'high-performance',
    antialias: true,
    alpha: true
  })
  const element = renderer.domElement
  element.id = 'menu-scene'

  const camera = new THREE.PerspectiveCamera(vFov, aspect, NEAR, FAR)
  camera.position.set(0, 20, 220);
  camera.lookAt(0, 0, 0);

  new OrbitControls(camera, element)

  const scene = new Scene(links)

  const canvas = { renderer, camera, scene, element }

  addResizeListener(canvas, getSize)
  setSize(canvas, width, height)

  animate(canvas)

  return canvas
}

exports.setActive = ({ scene }) => (id) => scene.setActive(id)

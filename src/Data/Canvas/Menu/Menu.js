const THREE = require('three')
const { addResizeListener, setSize, animate } = require('../lib/canvas')
const { Scene } = require('./Scene')

const getSize = () => ({
  width: Math.min(window.innerWidth - 30, 600),
  height: 80
})

exports.mkCanvas = () => {
  const { width, height } = getSize()

  const renderer = new THREE.WebGLRenderer({
    powerPreference: 'high-performance',
    antialias: true
  })
  const element = renderer.domElement
  element.id = 'menu-scene'

  const camera = new THREE.OrthographicCamera(
      width / 2, width / 2,
      height / 2, height / 2,
      1, 1000
    )

  const scene = new Scene()

  const canvas = { renderer, camera, scene, element }

  addResizeListener(canvas, getSize)
  setSize(canvas, width, height)

  animate(canvas)

  return canvas
}

exports.setLinks = ({ scene }) => (links) => scene.setLinks(links)

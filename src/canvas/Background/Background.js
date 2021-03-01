const theme = require('three-playground/src/theme/config').default
const { Canvas } = require('three-playground/src/Canvas')
const { Effects } = require('three-playground/src/theme/Effects')
const { Scene } = require('./Scene')

exports.mkCanvas = function mkCanvas() {
  const canvas = new Canvas(window.innerWidth, window.innerHeight, theme)
  const scene = new Scene(canvas)
  const composer = new Effects(scene, canvas)

  canvas.element.id = 'background-scene'

  window.addEventListener(
    'resize',
    () => {
      canvas.setSize(window.innerWidth, window.innerHeight)
      composer.setSize(window.innerWidth, window.innerHeight)
    }
  )

  window.Scroller.addEventListener(({progress}) => {
    scene.progress = progress
  })

  function animate() {
    composer.render()
    requestAnimationFrame(animate)
  }

  animate()

  return canvas.element
}

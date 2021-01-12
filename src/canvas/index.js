import { ScrollTrigger } from 'gsap/ScrollTrigger'
import { Canvas } from './three-playground/src/Canvas'
import { Effects } from './three-playground/src/theme/Effects'
import theme from './three-playground/src/theme/config'
import { Scene } from './Scene'

function main() {
  const canvas = new Canvas(window.innerWidth, window.innerHeight, theme)
  const scene = new Scene(canvas)
  const composer = new Effects(scene, canvas)

  canvas.element.id = 'scene'

  document.body.appendChild(canvas.element)

  window.addEventListener(
    'resize',
    () => {
      canvas.setSize(window.innerWidth, window.innerHeight)
      composer.setSize(window.innerWidth, window.innerHeight)
    }
  )

  window.onScroll = ({progress}) => {
    scene.progress = progress
  }

  function animate() {
    composer.render()
    requestAnimationFrame(animate)
  }

  animate()
}

export default { main }

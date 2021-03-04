import * as THREE from 'three'

export const mkRayCaster = (canvas, onChange) => {
  const rayCaster = new THREE.Raycaster()
  const mouse = new THREE.Vector2()

  const listener = (e) => {
    const {width, height} = canvas.element
    mouse.set((e.offsetX / width) * 2 - 1, (e.offsetY / height) * -2 + 1)
    rayCaster.setFromCamera(mouse, canvas.camera)
    if (onChange) onChange(rayCaster, mouse)
  }

  canvas.element.addEventListener('mousemove', listener)
  canvas.listeners.push(() =>
    canvas.element.removeEventListener('mousemove', listener)
  )

  return rayCaster
}

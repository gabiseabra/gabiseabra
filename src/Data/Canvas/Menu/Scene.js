import * as THREE from 'three'
import {Navbar} from '../../../lib/3d/objects/Navbar'

export class Scene extends THREE.Scene {
  constructor(links) {
    super()

    this.add(new WorldLight())

    const nav = new Navbar(links)
    this.add(nav)

    const lightPivot = new THREE.Object3D()
    this.add(lightPivot)

    const light = new THREE.RectAreaLight(0xff00ff, 15, 200, 5)
    light.position.set(0, 0, 50)
    lightPivot.add(light)

    this.nav = nav
    this.lightPivot = lightPivot
    this.progress = 0
  }

  get animating() {
    return (
      this.needsUpdate ||
      this.nav.children.some((link) => link.tween.isActive())
    )
  }

  set progress(p) {
    this.needsUpdate = true
    this.lightPivot.rotation.x = THREE.MathUtils.lerp(
      Math.PI / 3,
      -Math.PI / 3,
      p
    )
  }

  onBeforeRender() {
    this.needsUpdate = false
  }

  setActive(id) {
    this.nav.activeId = id
  }
}

export class WorldLight extends THREE.Object3D {
  constructor() {
    super()

    const ambient = new THREE.AmbientLight(0xffaac4, 0.75)
    this.add(ambient)

    const point = new THREE.PointLight(0xffaac4, 0.5)
    point.position.set(0, 100, 400)
    this.add(point)
  }
}

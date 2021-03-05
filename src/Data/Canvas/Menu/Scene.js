import * as THREE from 'three'
import {Navbar} from '../../../lib/3d/objects/Navbar'

export class Scene extends THREE.Scene {
  refs = {}

  constructor(links) {
    super()

    this.add(new WorldLight())

    const nav = new Navbar(links)
    this.add(nav)

    this.nav = nav
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

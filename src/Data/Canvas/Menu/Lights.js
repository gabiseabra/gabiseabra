import * as THREE from 'three'

export class WorldLight extends THREE.Object3D {
  name = 'worldLight'

  constructor() {
    super()

    const ambient = new THREE.AmbientLight(0xffaac4, 0.75)
    this.add(ambient)

    const point = new THREE.PointLight(0xffaac4, 0.5)
    point.position.set(0, 100, 400)
    this.add(point)
  }
}

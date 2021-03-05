import * as THREE from 'three'
import {mkTitleMesh} from './TitleMesh'

export class Title extends THREE.Object3D {
  constructor(ctx) {
    super()

    mkTitleMesh().then((mesh) => this.init(mesh, ctx))
  }

  init(mesh, {config}) {
    const pivot = new THREE.Object3D()
    pivot.rotateX(-1.17)
    this.add(pivot)

    mesh.translateY(config.sun.distance / 2).rotateX(-1.5)
    mesh.scale.x *= -1
    pivot.add(mesh)

    this.mesh = mesh
    this.pivot = pivot

    this.ready = true
  }

  setProgress(p, d) {
    if (!this.ready) return
    this.mesh.rotateX((Math.PI / 2) * d)
  }
}

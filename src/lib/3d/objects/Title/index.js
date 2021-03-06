import * as THREE from 'three'
import {mkTitleMesh} from './TitleMesh'

export class Title extends THREE.Object3D {
  constructor(ctx) {
    super()
    this.rotateX(-Math.PI * 0.37)

    mkTitleMesh().then((mesh) => this.init(mesh, ctx))
  }

  init(mesh, {config}) {
    const pivot = new THREE.Object3D()
    this.add(pivot)

    mesh.translateY(config.sun.distance / 2).rotateX(-1.5)
    mesh.scale.x *= -1
    pivot.add(mesh)

    this.mesh = mesh
    this.pivot = pivot

    this.ready = true
  }

  setProgress(p) {
    if (!this.ready) return
    this.pivot.rotation.x = THREE.MathUtils.lerp(0, Math.PI / 3, p)
  }
}

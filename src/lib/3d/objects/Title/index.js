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

    mesh.scale.x *= -1
    mesh.translateZ(config.sun.distance / 2)
    pivot.add(mesh)

    this.mesh = mesh
    this.pivot = pivot

    this.ready = true
    this.setProgress(0)
  }

  setProgress(p) {
    if (!this.ready) return
    this.pivot.rotation.x = THREE.MathUtils.lerp(
      -Math.PI / 1.95,
      -Math.PI / 1.5,
      p
    )
  }
}

import * as THREE from 'three'
import { Scene as BaseScene } from './three-playground/src/Scene'
import { Title } from './objects/Title'

const PROGRESS = Symbol('PROGRESS')

const lerp = (v0, v1, t) => v0 * (1 - t) + v1 * t

export class Scene extends BaseScene {
  cameraAxis = new THREE.Vector3(1, 0, 0)

  constructor(ctx) {
    super(ctx)

    this.titleDistance = this.sunDistance / 2

    const titlePivot = new THREE.Object3D()
    titlePivot.rotateX(-1.17)
    this.add(titlePivot)

    const title = new Title()
    title.translateY(this.titleDistance).rotateX(-1.5)
    title.scale.x *= -1;
    titlePivot.add(title)

    this.terrain = this.getObjectByName('terrain')
    this.title = title
    this.progress = 0
  }

  set progress(n) {
    const d = (this[PROGRESS] || 0) - n
    this[PROGRESS] = n
    this.angle = lerp(.35, Math.PI, n)
    this.terrain.visible = n > .5
    this.title.rotateX((Math.PI / 2) * d)
  }

  updateCamera() {
    super.updateCamera()
    this.camera.rotateZ(Math.PI)
  }
}

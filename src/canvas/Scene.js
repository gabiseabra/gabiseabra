import * as THREE from 'three'
import { Scene as BaseScene } from './three-playground/src/Scene'

const lerp = (v0, v1, t) => v0 * (1 - t) + v1 * t

export class Scene extends BaseScene {
  cameraAxis = new THREE.Vector3(1, 0, 0)

  constructor(ctx) {
    super(ctx)

    this.terrain = this.getObjectByName('terrain')
    this.progress = 0
  }

  set progress(n) {
    this.angle = lerp(.35, Math.PI, n)
    this.terrain.visible = n > .5
  }

  updateCamera() {
    super.updateCamera()
    this.camera.rotateZ(Math.PI)
  }
}

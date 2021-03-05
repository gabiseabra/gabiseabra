import * as THREE from 'three'
import {Scene as BaseScene} from 'three-playground/src/Scene'
import {Title} from '../../../lib/3d/objects/Title'

const PROGRESS = Symbol('PROGRESS')

export class Scene extends BaseScene {
  cameraAxis = new THREE.Vector3(1, 0, 0)

  constructor(ctx) {
    super(ctx)

    this.title = new Title(ctx)
    this.add(this.title)

    this.terrain = this.getObjectByName('terrain')

    this.progress = 0
  }

  set progress(n) {
    const d = (this[PROGRESS] || 0) - n
    this[PROGRESS] = n

    this.title.setProgress(n, d)
    this.angle = THREE.MathUtils.lerp(0.35, Math.PI, n)
    this.terrain.visible = n > 0.5
  }

  updateCamera() {
    super.updateCamera()
    // fix z axis at pi
    this.camera.rotateZ(Math.PI)
  }
}

import * as THREE from 'three'
import cubicBezier from 'bezier-easing'
import {Sun} from 'three-playground/src/objects/Sun'
import {World} from 'three-playground/src/objects/World'
import {CameraController} from 'three-playground/src/objects/CameraController'
import {SunLight} from 'three-playground/src/theme/Lights'
import {Title} from '../../../lib/3d/objects/Title'

const PROGRESS = Symbol('PROGRESS')

const INITIAL_ANGLE = 0.35

const mieEasing = cubicBezier(0, 0.5, 0.5, 1)

const mkSun = (config) => {
  const pivot = new THREE.Object3D()
  const sun = new Sun(config.sun)
  sun.position.z = -config.sun.distance
  pivot.add(sun)

  return pivot
}

export class Scene extends THREE.Scene {
  animating = true

  constructor(ctx) {
    super(ctx)

    this.title = new Title(ctx)
    this.add(this.title)

    this.world = new World(ctx.config)
    this.world.remove(this.world.sunPivot)
    this.add(this.world)

    this.cameraController = new CameraController(ctx.camera, ctx.config)
    this.cameraController.axis = new THREE.Vector3(1, 0, 0)
    ctx.camera.rotateZ(Math.PI)
    this.add(this.cameraController)

    this.sunLight = new SunLight()
    this.sunLight.position.z = ctx.config.sun.distance
    this.add(this.sunLight)

    const sun0 = mkSun(ctx.config)
    const sun1 = mkSun(ctx.config)
    this.add(sun0, sun1)

    this.terrain = this.getObjectByName('terrain')
    this.suns = {top: sun0, bottom: sun1}
    this.progress = 0
  }

  set progress(t) {
    const d = (this[PROGRESS] || 0) - t
    const angle = THREE.MathUtils.lerp(INITIAL_ANGLE, Math.PI, t)
    this[PROGRESS] = t

    this.cameraController.angle = angle
    this.world.angle = angle
    this.title.setProgress(t, d)

    this.sunLight.position.copy(this.world.sunPosition)

    const sunTopPos = Math.min(t, 0.3) / 0.2
    const sunBottomPos = (1 - Math.max(t, 0.7)) / 0.2
    this.suns.top.rotation.x = THREE.MathUtils.lerp(0.35, 0.45, sunTopPos)
    this.suns.bottom.rotation.x =
      Math.PI * THREE.MathUtils.lerp(0.97, 0.955, sunBottomPos)

    const mieT = mieEasing(Math.abs((t - 0.5) * 2))
    this.world.sky.material.uniforms.mieCoefficient.value = THREE.MathUtils.lerp(
      0.0005,
      0.0035,
      mieT
    )
    this.world.sky.material.uniforms.mieDirectionalG.value = THREE.MathUtils.lerp(
      0.45,
      0.8,
      mieT
    )

    this.terrain.visible = t > 0.5
  }
}

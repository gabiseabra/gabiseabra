import * as THREE from 'three'

const {
  EffectComposer,
  RenderPass,
  EffectPass,
  BlendFunction,
  RealisticBokehEffect
} = require('postprocessing')

const DOF = [
  new THREE.Vector4(6.77, 0, 0.001, 0.8),
  new THREE.Vector4(0.04, 0.02, 0.01, 0.35),
  new THREE.Vector4(0, 0.15, 0.062, 0.31)
]

export class Effects extends EffectComposer {
  constructor({scene, camera, renderer, width, height}) {
    super(renderer, {
      multisampling: 0,
      frameBufferType: THREE.HalfFloatType
    })

    this.addPass(new RenderPass(scene, camera))
    const bokeh = new RealisticBokehEffect({
      blendFunction: BlendFunction.NORMAL,
      focus: 0.13,
      luminanceThreshold: 3,
      luminanceGain: 1,
      focalLength: 10,
      fStop: 0,
      bias: 0,
      fringe: 1.4,
      maxBlur: 6.0,
      rings: 4,
      samples: 3,
      manualDoF: true,
      showFocus: false
    })
    this.addPass(new EffectPass(camera, bokeh))
    this.bokeh = bokeh
    this.dof = bokeh.uniforms.get('dof').value
    this.dof.copy(DOF[0])

    this.uniforms = {
      focus: bokeh.uniforms.get('focus'),
      fringe: bokeh.uniforms.get('fringe'),
      maxBlur: bokeh.uniforms.get('maxBlur'),
      dof: bokeh.uniforms.get('dof')
    }

    this.setSize(width, height)
  }

  set progress(t) {
    const n = (t - 0.5) * 2
    const tHalf = Math.abs(n)

    if (n < 0) {
      this.uniforms.dof.value.lerpVectors(DOF[0], DOF[1], tHalf)
      this.uniforms.focus.value = THREE.MathUtils.lerp(0.13, 0.06, tHalf)
    } else {
      this.uniforms.dof.value.lerpVectors(DOF[1], DOF[2], tHalf)
      this.uniforms.focus.value = THREE.MathUtils.lerp(0.06, 0.016, tHalf)
    }
    this.uniforms.fringe.value = THREE.MathUtils.lerp(0, 10, t)
    this.uniforms.maxBlur.value = THREE.MathUtils.lerp(10, 4, tHalf)
  }
}

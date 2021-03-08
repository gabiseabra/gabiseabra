import * as THREE from 'three'

const {
  EffectComposer,
  RenderPass,
  EffectPass,
  BlendFunction,
  RealisticBokehEffect
} = require('postprocessing')

const DOF = [
  new THREE.Vector4(6.77, 0, 0.01, 0.2),
  new THREE.Vector4(0.04, 0.02, 0.01, 0.35),
  new THREE.Vector4(0.04, 0.5, 0, 0)
]

export class Effects extends EffectComposer {
  constructor({scene, camera, renderer, width, height}) {
    super(renderer, {
      multisampling: 0,
      frameBufferType: THREE.HalfFloatType
    })

    this.addPass(new RenderPass(scene, camera))
    // const normal = new NormalPass(scene, camera)
    // this.addPass(normal)
    const bokeh = new RealisticBokehEffect({
      blendFunction: BlendFunction.NORMAL,
      focus: 0.13,
      luminanceThreshold: 3,
      luminanceGain: 1,
      focalLength: 10,
      fStop: 0,
      bias: 0,
      fringe: 1.4,
      maxBlur: 4.0,
      rings: 4,
      samples: 6,
      manualDoF: true,
      showFocus: false
    })
    this.addPass(new EffectPass(camera, bokeh))
    this.bokeh = bokeh
    this.dof = bokeh.uniforms.get('dof').value
    this.dof.copy(DOF[0])

    this.uniforms = {
      focus: bokeh.uniforms.get('focus'),
      focalLength: bokeh.uniforms.get('focalLength'),
      fStop: bokeh.uniforms.get('fStop'),
      luminanceThreshold: bokeh.uniforms.get('luminanceThreshold'),
      luminanceGain: bokeh.uniforms.get('luminanceGain'),
      bias: bokeh.uniforms.get('bias'),
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
      this.uniforms.focus.value = THREE.MathUtils.lerp(0.06, 0.24, tHalf)
    }
    this.uniforms.fringe.value = THREE.MathUtils.lerp(0, 10, t)
    this.uniforms.maxBlur.value = THREE.MathUtils.lerp(10, 4, tHalf)
  }

  getGUI() {
    return [
      ['uniforms.focus', {min: 0, max: 4, step: 0.01}],
      ['uniforms.focalLength', {min: 0, max: 10, step: 0.1}],
      ['uniforms.fStop', {min: 0, max: 10, step: 0.1}],
      ['uniforms.luminanceThreshold', {min: 0, max: 10, step: 0.1}],
      ['uniforms.luminanceGain', {min: 0, max: 10, step: 0.1}],
      ['uniforms.bias', {min: 0, max: 10, step: 0.1}],
      ['uniforms.fringe', {min: 0, max: 10, step: 0.1}],
      ['uniforms.maxBlur', {min: 0, max: 10, step: 0.1}],
      ['dof', {min: 0, max: 10, step: 0.001}]
    ]
  }
}
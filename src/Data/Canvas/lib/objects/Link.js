import * as THREE from 'three'
import {gsap} from 'gsap'
import SourceCodePro from '../fonts/SourceCodePro'

const DURATION = 0.25

const COLOR = new THREE.Color(0x73b6fa)
const ACTIVE_COLOR = new THREE.Color(0xffbcb8)

const rgb = ({r, g, b}) => ({r, g, b})

export class Link extends THREE.Object3D {
  active = false
  chars = []

  constructor(label, charWidth = 8.45) {
    super()

    const mat = new THREE.MeshPhysicalMaterial({color: COLOR})
    // const mat = new THREE.MeshNormalMaterial()

    label.split('').forEach((char, idx) => {
      const geom = new THREE.TextGeometry(char, {
        font: SourceCodePro,
        size: 10,
        height: 5,
        curveSegments: 8,
        bevelEnabled: true,
        bevelThickness: 0.8,
        bevelSize: 0.5,
        bevelOffset: -0.5,
        bevelSegments: 4
      })
      const mesh = new THREE.Mesh(geom, mat)
      mesh.translateX(charWidth * idx)

      const pivot = new THREE.Object3D()
      pivot.add(mesh)
      this.add(pivot)
      this.chars.push(mesh)
    })

    const scale = this.chars.map((c) => c.scale)

    this.material = mat
    this.tween = gsap
      .timeline({defaults: {duration: DURATION}})
      .fromTo(mat.color, rgb(COLOR), rgb(ACTIVE_COLOR), 0)
      .fromTo(scale, {z: 1}, {z: 3}, 0)
      .pause(0)
  }

  setActive(active) {
    if (this.active == active) return
    if (active) this.tween.play()
    else this.tween.reverse()
    this.active = active
  }
}

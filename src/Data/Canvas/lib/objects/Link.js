import * as THREE from 'three'
import {gsap} from 'gsap'

/* link looks & feel */
const DURATION = 0.25
const COLOR = new THREE.Color(0x73b6fa)
const ACTIVE_COLOR = new THREE.Color(0xffbcb8)

const rgb = ({r, g, b}) => ({r, g, b})

export class Link extends THREE.Mesh {
  constructor(geom) {
    super(geom, new THREE.MeshPhysicalMaterial({color: COLOR}))

    this.tween = gsap
      .timeline({defaults: {duration: DURATION}})
      .fromTo(this.material.color, rgb(COLOR), rgb(ACTIVE_COLOR), 0)
      .pause(0)
  }

  setActive(active) {
    if (this.active == active) return
    if (active) this.tween.play()
    else this.tween.reverse()
    this.active = active
  }
}

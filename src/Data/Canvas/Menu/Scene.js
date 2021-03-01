import * as THREE from 'three'
import { Flow } from 'three/examples/jsm/modifiers/CurveModifier'
import { Link } from '../lib/objects/Link'

const UP = new THREE.Vector3(0, 0, 1)

const CHAR_WIDTH = 3.45
const SPACING = 3
const ANGLE = Math.PI * .25

export class Scene extends THREE.Scene {
  refs = {}

  constructor(links) {
    super()

    const spaces = links.length - 1
    let chars = 0

    const nav = new THREE.Object3D()
    this.add(nav)

    links.forEach(({ id, label }) => {
      const link = new Link(label, 0)

      this.refs[id] = link
      nav.add(link)

      chars += label.length
    })

    const length = chars + spaces * SPACING

    const chord = length * CHAR_WIDTH * 2
    const radius = (chord / 2) / Math.sin(ANGLE / 2)
    const sagitta = radius - Math.sqrt(Math.pow(radius, 2) - Math.pow(chord / 2, 2))

    this.translateX(-CHAR_WIDTH / 2)
    this.translateZ(radius - sagitta)

    nav.children.reduce((i, link, idx) =>
      link.children.reduce((j, char) => {
        const pos = j + (idx * SPACING)
        const u = pos / (length - 1)
        const a = -ANGLE * (u - .5)
        char.rotateY(a)
        char.translateZ(-radius)
        return j + 1
      }, i)
    , 0)
  }

  setActive(id) {
    console.log(id)
  }
}

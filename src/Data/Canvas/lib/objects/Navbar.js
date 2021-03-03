import * as THREE from 'three'
import {Link} from './Link'

const CHAR_WIDTH = 3.45
const SPACING = 3
const ANGLE = Math.PI * 0.25

export class Navbar extends THREE.Object3D {
  refs = {}

  constructor(links) {
    super()

    let length // number of characters in the scene including spaces
    {
      const spaces = links.length - 1
      let chars = 0

      links.forEach(({id, label}) => {
        const link = new Link(label, 0)

        this.refs[id] = link
        this.add(link)

        chars += label.length
      })

      length = chars + spaces * SPACING
    }

    // arc params
    const chord = length * CHAR_WIDTH * 2
    const radius = chord / 2 / Math.sin(ANGLE / 2)
    const sagitta =
      radius - Math.sqrt(Math.pow(radius, 2) - Math.pow(chord / 2, 2))

    // center
    this.translateX(-CHAR_WIDTH / 2)
    // set pivot
    this.translateZ(radius - sagitta)

    // iterate over each character to translate around the arc
    this.children.reduce(
      (i, link, idx) =>
        link.children.reduce((j, char) => {
          const pos = j + idx * SPACING
          const u = pos / (length - 1)
          const a = -ANGLE * (u - 0.5)
          char.rotateY(a)
          char.translateZ(-radius)
          return j + 1
        }, i),
      0
    )
  }

  setActive(id) {
    if (this.activeId) {
      this.refs[this.activeId].setActive(false)
    }
    if (this.refs[id]) {
      this.refs[id].setActive(true)
      this.activeId = id
    }
  }
}

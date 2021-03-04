import * as THREE from 'three'
import {BufferGeometryUtils} from 'three/examples/jsm/utils/BufferGeometryUtils'
import SourceCodePro from '../fonts/SourceCodePro'
import {Link} from './Link'

/* curve modifier options */
const CHAR_WIDTH = 3.5 // 8.45
const SPACING = 2.5 // num of spaces between links
const ANGLE = Math.PI * 0.3 // angle of the arc

export class Navbar extends THREE.Object3D {
  refs = {}

  constructor(links) {
    super()

    const geomsList = links.map((l) => mkLinkGeometries(l))
    const chars = geomsList.reduce((acc, l) => l.length + acc, 0)
    const spaces = links.length - 1
    const length = chars + spaces * SPACING

    const curve = mkCurve(length)

    // modify and merge link geometries
    forEach2(geomsList, curve.modifyGeometry)
    geomsList.forEach((geoms, idx) => {
      const link = links[idx]

      const geom = new THREE.Geometry().fromBufferGeometry(
        BufferGeometryUtils.mergeBufferGeometries(geoms)
      )
      geom.translate(0, 0, curve.apothem)

      const mesh = new Link(geom)
      const spacing = idx * SPACING * CHAR_WIDTH
      mesh.translateX(spacing)
      mesh.name = link.id

      // var bbox = new THREE.BoundingBoxHelper(mesh, 0xff0000)
      // bbox.update()
      // this.add(bbox)

      this.refs[link.id] = mesh
      this.add(mesh)
    })

    this.translateX(-((spaces + 1) * SPACING * CHAR_WIDTH) / 2)
  }

  setActive(id) {
    if (this.activeId) {
      this.refs[this.activeId].setActive(false)
      this.activeId = null
    }
    if (this.refs[id]) {
      this.refs[id].setActive(true)
      this.activeId = id
    }
  }
}

const mkLinkGeometries = ({label}) =>
  label.split('').map(
    (char) =>
      new THREE.TextBufferGeometry(char, {
        font: SourceCodePro,
        size: 10,
        height: 7,
        curveSegments: 8,
        bevelEnabled: true,
        bevelThickness: 0.8,
        bevelSize: 0.5,
        bevelOffset: -0.5,
        bevelSegments: 4
      })
  )

const mkCurve = (length) => {
  // arc
  const chord = length * CHAR_WIDTH * 2
  const radius = chord / 2 / Math.sin(ANGLE / 2)
  const sagitta =
    radius - Math.sqrt(Math.pow(radius, 2) - Math.pow(chord / 2, 2))
  const apothem = radius - sagitta
  return {
    chord,
    radius,
    sagitta,
    apothem,
    getOffset() {},
    modifyGeometry(geom, j, idx0) {
      const mesh = new THREE.Mesh(geom)

      const pos = j + idx0 * SPACING
      const u = pos / (length - 1)
      const a = -ANGLE * (u - 0.5)

      mesh.rotateY(a)
      mesh.translateZ(-radius)
      mesh.updateMatrix()

      geom.applyMatrix(mesh.matrix)
      geom.userData.offset = Math.cos(a) * radius
    }
  }
}

const forEach2 = (l, fun) =>
  l.reduce(
    (i, ll, idx0) =>
      ll.reduce((j, x, idx1) => {
        fun(x, j, idx0, idx1)
        return j + 1
      }, i),
    0
  )

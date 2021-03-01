import * as THREE from 'three'
import SourceCodePro from '../fonts/SourceCodePro'

export class Link extends THREE.Object3D {
  constructor(label, charWidth = 8.45) {
    super()

    const mat = new THREE.MeshNormalMaterial()

    this.width = label.length * charWidth

    label.split('').forEach((char, idx) => {
      const geom = new THREE.TextGeometry(char, {
        font: SourceCodePro,
        size: 10,
        height: 10
      })
      const mesh = new THREE.Mesh(geom, mat)
      mesh.translateX(charWidth * idx)
      this.add(mesh)
    })
  }
}

import * as THREE from 'three'
import { STLLoader } from 'three/examples/jsm/loaders/STLLoader';
import { IridescentMaterial } from '../materials/Iridescent'

const noop = () => null

const mat = new IridescentMaterial(400, 1.5, 4.5, 1)

export class Title extends THREE.Object3D {
  constructor() {
    super()

    new Promise((resolve, reject) =>
      new STLLoader().load('/models/my_name_thin.stl', resolve, noop, reject)
    ).then((geom) => {
      const scale = 3
      geom.scale(scale, scale, -scale)
      this.add(new THREE.Mesh(geom, mat))
    })
  }
}

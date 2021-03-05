import * as THREE from 'three'
import {STLLoader} from 'three/examples/jsm/loaders/STLLoader'
import {IridescentMaterial} from '../../materials/Iridescent'

const SCALE = 3
const STL_URL = '/models/my_name_thin.stl'
const MATERIAL = new IridescentMaterial(400, 1.5, 4.5, 1)

const noop = () => null

export class TitleMesh extends THREE.Mesh {
  constructor(geom) {
    geom.scale(SCALE, SCALE, -SCALE)
    super(geom, MATERIAL)
  }
}

export const mkTitleMesh = () => {
  return new Promise((resolve, reject) =>
    new STLLoader().load(STL_URL, resolve, noop, reject)
  ).then((geom) => new TitleMesh(geom))
}

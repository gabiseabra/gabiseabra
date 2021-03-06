import * as THREE from 'three'
import {IridescentMaterial as Base} from './IridescentMaterial'
import {ThinFilmFresnelMap} from './ThinFilmFresnelMap'

const cubeMap = (name) =>
  new THREE.CubeTextureLoader().load([
    `/img/skybox/${name}/posX.jpg`,
    `/img/skybox/${name}/negX.jpg`,
    `/img/skybox/${name}/posY.jpg`,
    `/img/skybox/${name}/negY.jpg`,
    `/img/skybox/${name}/posZ.jpg`,
    `/img/skybox/${name}/negZ.jpg`
  ])

const radiance = cubeMap('radiance')
const irradiance = cubeMap('irradiance')

export class IridescentMaterial extends Base {
  constructor(
    filmThickness,
    refractiveIndexFilm,
    refractiveIndexBase,
    size = 1
  ) {
    super(
      irradiance,
      radiance,
      new ThinFilmFresnelMap(
        filmThickness,
        refractiveIndexFilm,
        refractiveIndexBase,
        size
      )
    )
  }
}

import * as THREE from 'three'
import {Navbar} from '../lib/objects/Navbar'
import {WorldLight} from './Lights'

export class Scene extends THREE.Scene {
  refs = {}

  constructor(links) {
    super()

    this.add(new WorldLight())

    const nav = new Navbar(links)
    this.add(nav)

    this.nav = nav
  }
}

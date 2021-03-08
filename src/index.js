import {gsap} from 'gsap'
import {ScrollTrigger} from 'gsap/ScrollTrigger'
import * as SMAA from './lib/3d/smaa'
import Main from './Main'
import './index.scss'

gsap.registerPlugin(ScrollTrigger)

document.addEventListener('DOMContentLoaded', () => {
  //SMAA.init()
  Main.main()
})

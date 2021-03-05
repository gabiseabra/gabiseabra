import {gsap} from 'gsap'
import {ScrollTrigger} from 'gsap/ScrollTrigger'
import Main from './Main'
import './lib/chart'
import './index.scss'

gsap.registerPlugin(ScrollTrigger)

document.addEventListener('DOMContentLoaded', () => {
  Main.main()
})

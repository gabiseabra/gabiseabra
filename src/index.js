import * as scroller from './lib/scroller'
import Main from './Main'
import './index.scss'

document.addEventListener('DOMContentLoaded', () => {
  scroller.init()
  Main.main()
})

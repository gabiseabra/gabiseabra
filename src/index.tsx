//import Main from './Main'
import './styles/main.css'
import ReactDOM from 'react-dom'

import {Blob} from '@/components/SVG/Blob'
import {Menu} from '@/components/Menu'

function GooFilter() {
  return (
    <filter id="goo">
      <feGaussianBlur in="SourceGraphic" stdDeviation="40" result="blur" />
      <feColorMatrix
        in="blur"
        type="matrix"
        values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 75 -10"
        result="goo"
      />
      <feBlend in="SourceGraphic" in2="goo" />
    </filter>
  )
}

function Main(): JSX.Element {
  return (
    <>
      <Menu />
      <div className="eyy" style={{filter: 'url(#goo)'}}>
        <div className="lmao">
          <p>lmaooooo</p>
        </div>
      </div>
      <svg
        className="defs"
        viewBox={`0 0 ${window.innerWidth} ${window.innerHeight}`}
        enableBackground={`new 0 0 ${window.innerWidth} ${window.innerHeight}`}
        width="0"
        height="0"
        preserveAspectRatio="xMidYMid slice"
      >
        <defs>
          <clipPath id="shape" clipPathUnits="objectBoundingBox">
            <Blob />
          </clipPath>
          <GooFilter />
        </defs>
      </svg>
    </>
  )
}

ReactDOM.render(<Main />, document.getElementById('root'))

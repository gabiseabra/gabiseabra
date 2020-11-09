import {useEffect} from 'react'
import {animated, useSpring} from 'react-spring'

function usePointerAnimation() {
  const [pointer, setPointer] = useSpring(() => ({
    xy: [0.5, 0.5],
    size: 1
  }))
  useEffect(() => {
    window.addEventListener('mousemove', (e) => {
      const capxy = (lim) => (d) => Math.min(1 - lim, Math.max(lim, d))
      const x = capxy(0.3)(e.clientX / window.innerWidth)
      const y = capxy(0.2)(e.clientY / window.innerHeight)
      const center = (d) =>
        Math.min(1, Math.max(0, -8 * Math.pow(d, 2) + 8 * d - 1))
      const size = (center(x) + center(y)) / 2
      setPointer({xy: [x, y], size})
    })
  })
  return pointer
}

export function PointerBlob(): JSX.Element {
  const pointer = usePointerAnimation()
  return (
    <animated.circle
      cx="0"
      cy="0"
      fill="black"
      stroke="black"
      transform={pointer.xy.interpolate((x, y) => `translate(${x} ${y})`)}
      r={pointer.size.interpolate((x) => x * 0.25)}
    />
  )
}

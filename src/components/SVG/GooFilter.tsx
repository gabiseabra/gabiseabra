export function GooFilter(): JSX.Element {
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

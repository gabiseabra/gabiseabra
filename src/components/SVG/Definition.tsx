import {ReactNode} from 'react'

interface DefinitionProps {
  children: ReactNode
  width: number
  height: number
}

export function Definition({
  children,
  width,
  height
}: DefinitionProps): JSX.Element {
  return (
    <svg
      className="defs"
      viewBox={`0 0 ${width} ${height}`}
      enableBackground={`new 0 0 ${width} ${height}`}
      width="0"
      height="0"
      preserveAspectRatio="xMidYMid slice"
    >
      <defs>{children}</defs>
    </svg>
  )
}

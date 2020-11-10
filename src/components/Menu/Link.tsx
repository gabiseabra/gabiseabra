import styles from './styles.css'

interface LinkProps {
  children: string
  to: string
  active?: boolean
}

export function Link({children, to, active}: LinkProps): JSX.Element {
  const classNames = [active && styles.active, styles.link]
    .filter(Boolean)
    .join(' ')
  return (
    <a href={to} className={classNames}>
      <svg viewBox={`0 0 ${children.length * 20 + 20} 60`}>
        <text x="50%" y="50%" dominantBaseline="middle" textAnchor="middle">
          {children.split('').map((char, i) => (
            <tspan key={`${char}-${i}`}>{char}</tspan>
          ))}
        </text>
      </svg>
    </a>
  )
}

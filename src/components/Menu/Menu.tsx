import {Link} from './Link'
import styles from './styles.css'

export function Menu(): JSX.Element {
  return (
    <nav className={styles.nav}>
      <Link active to="#home">
        HOME
      </Link>
      <Link to="#skillz">SKILLZ</Link>
      <Link to="#projects">PROJECTS</Link>
    </nav>
  )
}

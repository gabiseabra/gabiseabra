import styles from './styles.css'

export function Blob(): JSX.Element {
  return (
    <>
      <circle
        cx="0.5"
        cy="0.5"
        fill="black"
        stroke="black"
        r=".35"
        className={styles.move0}
      />
      <circle
        cx="0.5"
        cy="0.5"
        fill="black"
        stroke="black"
        r=".3"
        className={styles.move1}
      />
      <circle
        cx="0.5"
        cy="0.5"
        fill="black"
        stroke="black"
        r=".25"
        className={styles.move2}
      />
      <circle
        cx="0.5"
        cy="0.5"
        fill="black"
        stroke="black"
        r=".1"
        className={styles.move3}
      />
    </>
  )
}

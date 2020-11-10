import {Definition} from './Definition'
import {GooFilter} from './GooFilter'

export function RootDefinitions(): JSX.Element {
  return (
    <Definition width={window.innerWidth} height={window.innerHeight}>
      <GooFilter />
    </Definition>
  )
}

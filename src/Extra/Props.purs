module Hey.Extra.Props where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Record.Unsafe.Union (unsafeUnion)

data PropsRep (required :: # Type) (optional :: # Type) = DefProps (Record optional)

runProps :: forall props required optional opts any out 
  . Row.Union required optional out
  => Row.Union required opts props
  => Row.Union opts any optional
  => PropsRep required optional
  -> {|props}
  -> {|out}
runProps (DefProps def) = flip unsafeUnion $ def

{-----------------}

type Required r =
  ( a :: String
  | r
  )

type Optional =
  ( b :: String
  , c :: Int
  )

type TestProps = PropsRep (Required ()) Optional

testProps = DefProps
  { b: "lmao"
  , c: 2
  } :: TestProps

class PartialRow (opts :: # Type) (optional :: # Type) | optional -> opts
instance partialRowImpl :: ( Row.Union x r r ) => PartialRow x r

test :: forall opts opts' . Row.Union opts opts' Optional => Record (Required opts) -> String
test = runProps testProps >>> \{ a, b, c } -> a <> " " <> b <>  " " <> show c

main :: Effect Unit
main = do
  log $ test { a : "eyy" }
  log $ test { a : "eyy", c : 420 }

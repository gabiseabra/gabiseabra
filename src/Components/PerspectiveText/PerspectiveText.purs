module Hey.Components.PerspectiveText (mkPerspectiveText) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable)
import Data.Unfoldable (replicate)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Hey.Extra.Props (PropsRep(..), runProps)
import Prim.Row as Row
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, Ref, component, readRefMaybe, useEffectOnce)
import React.Basic.Hooks as React
import Web.DOM (Node)

foreign import styles :: Styles

type Styles =
  { container :: String
  , layers :: String
  , placeholder :: String
  }

foreign import animate :: EffectFn1 Node Unit

type RequiredPerspectiveTextProps r
  = ( text :: String
    | r
    )

type OptionalPerspectiveTextProps
  = ( targetRef :: Maybe (Ref (Nullable Node))
    , before :: JSX
    , after :: JSX
    )

type PerspectiveTextProps r = Record (RequiredPerspectiveTextProps r)

perspectiveTextProps =
  DefProps
    { targetRef: Nothing
    , before: mempty :: JSX
    , after: mempty :: JSX
    } ::
    PropsRep (RequiredPerspectiveTextProps ()) OptionalPerspectiveTextProps

mkPerspectiveText :: forall opts opts'. Row.Union opts opts' OptionalPerspectiveTextProps => Component (PerspectiveTextProps opts)
mkPerspectiveText =
  component "PerspectiveText"
  $ runProps perspectiveTextProps
  >>> \{ targetRef, text, before, after } -> React.do
    useEffectOnce
      $ maybe (pure Nothing) readRefMaybe targetRef
      >>= case _ of 
        Nothing -> pure mempty
        Just node -> do
          runEffectFn1 animate node
          pure mempty
    pure $ DOM.div
      { className: styles.container
      , children:
          [ DOM.div
            { className: styles.layers
            , children: replicate 15 $ DOM.span_ $ [ DOM.text text ]
            }
          , DOM.span
            { className: styles.placeholder
            , children: [ DOM.text text ]
            }
          ]
      }
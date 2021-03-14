module Hey.Components.Background (mkBackground) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Background as Background
import Hey.Hooks.UseScroller (useScroller)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffect, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Web.DOM.Node as Node

mkBackground :: forall a. Component a
mkBackground =
  component "Background"
    $ \_ -> React.do
        canvas /\ setCanvas <- useState Nothing
        scroller <- useScroller
        ref <- useRef null
        useEffectOnce
          $ readRefMaybe ref
          >>= maybe (pure mempty) \node -> do
              c <- Background.mkCanvas
              setCanvas $ const $ Just c
              void $ Node.appendChild (Canvas.toNode c) node
              pure
                $ do
                    setCanvas $ const Nothing
                    Canvas.destroy c
        useEffect scroller
          $ maybe (mempty) (flip Background.setScroller $ scroller) canvas
          *> pure mempty
        pure $ DOM.div { ref, id: "background" }

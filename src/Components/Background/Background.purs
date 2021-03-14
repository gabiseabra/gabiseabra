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
        bg /\ setBg <- useState Nothing
        scroller <- useScroller
        ref <- useRef null
        useEffectOnce
          $ readRefMaybe ref
          >>= maybe (pure mempty) \node -> do
              x <- Background.mkCanvas
              setBg $ const $ Just x
              void $ Node.appendChild (Canvas.toNode x) node
              pure
                $ do
                    setBg $ const Nothing
                    Canvas.destroy x
        useEffect scroller
          $ maybe (mempty) (flip Background.setScroller $ scroller) bg
          *> pure mempty
        pure $ DOM.div { ref, id: "background" }

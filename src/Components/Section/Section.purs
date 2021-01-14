module Hey.Components.Section (SectionProps, mkSection) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.Traversable (traverse)
import Effect (Effect)
import Hey.Hooks.UseScroll (useScrollerElement, useSnapPoint, useSnapPointRef)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, readRefMaybe, useRef)
import React.Basic.Hooks as React
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

foreign import styles :: Styles

type Styles
  = { content :: String
    }

type SectionProps
  = { key :: String
    , height :: String
    , children :: Array JSX
    , snap :: Maybe (HTMLElement -> Effect Number)
    }

mkSection :: Component SectionProps
mkSection =
  component "Section"
    $ \{ key, height, snap, children } -> React.do
        containerRef <- useRef null
        scroller <- useScrollerElement
        case snap of
          Nothing -> useSnapPointRef key containerRef
          Just f ->
            useSnapPoint key
              $ readRefMaybe containerRef
              >>= ((=<<) HTMLElement.fromNode)
              >>> traverse f
        let
          style = DOM.css { height }

          content = DOM.div { style, className: styles.content, children }
        pure
          $ DOM.section
              { style
              , ref: containerRef
              , children: pure $ maybe mempty (DOM.createPortal content) scroller
              }

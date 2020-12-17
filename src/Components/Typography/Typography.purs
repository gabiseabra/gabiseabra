module Hey.Components.Typography
  ( Heading(..)
  , h
  , p
  , p_
  , ParagraphProps
  , pProps
  , SpanProps
  , spanProps
  , span
  , span_
  ) where

import Prelude
import Data.Foldable (intercalate)
import Data.Monoid (guard)
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX)

foreign import styles :: Styles

type Styles
  = { heading :: String
    , paragraph :: String
    , span :: String
    }

data Heading
  = H1
  | H2
  | H3
  | H4

derive instance eqHeading :: Eq Heading

h :: Heading -> Array JSX -> JSX
h = flip $ \children -> component >>> ((#) { className: styles.heading, children })
  where
  component x
    | x == H1 = DOM.h1
    | x == H2 = DOM.h2
    | x == H3 = DOM.h3
    | otherwise = DOM.h4

type ParagraphProps
  = { highlight :: Boolean
    }

pProps =
  { highlight: false
  } ::
    ParagraphProps

p :: ParagraphProps -> Array JSX -> JSX
p { highlight } children =
  DOM.p
    { className:
        intercalate " "
          $ pure styles.paragraph
          <> guard highlight [ "highlight" ]
    , children: children
    }

p_ :: Array JSX -> JSX
p_ = p pProps

type SpanProps
  = { highlight :: Boolean
    , bold :: Boolean
    }

spanProps =
  { highlight: false
  , bold: false
  } ::
    SpanProps

span :: SpanProps -> Array JSX -> JSX
span { highlight, bold } children =
  DOM.span
    { className:
        intercalate " "
          $ pure styles.span
          <> guard highlight [ "highlight" ]
          <> guard bold [ "bold" ]
    , children
    }

span_ :: Array JSX -> JSX
span_ = span spanProps

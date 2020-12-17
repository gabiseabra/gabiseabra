module Hey.Components.Typography
  ( Heading(..)
  , FontSize(..)
  , h
  , p
  , p_
  , ParagraphProps
  , pProps
  , SpanProps
  , spanProps
  , span
  , span_
  , mark
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
    , mark :: String
    }

data Heading
  = H1
  | H2
  | H3
  | H4

derive instance eqHeading :: Eq Heading

data FontSize
  = Small
  | Medium
  | Large

derive instance eqFontSize :: Eq FontSize

fontSizeCls :: FontSize -> String
fontSizeCls Small = "font-sm"

fontSizeCls Medium = "font-md"

fontSizeCls Large = "font-lg"

h :: Heading -> Array JSX -> JSX
h = flip $ \children -> component >>> ((#) { className: styles.heading, children })
  where
  component x
    | x == H1 = DOM.h1
    | x == H2 = DOM.h2
    | x == H3 = DOM.h3
    | otherwise = DOM.h4

type ParagraphProps
  = { fontSize :: FontSize
    , children :: Array JSX
    }

pProps =
  { children: mempty
  , fontSize: Medium
  } ::
    ParagraphProps

p :: ParagraphProps -> JSX
p { fontSize, children } =
  DOM.p
    { className:
        intercalate " "
          $ [ styles.paragraph
            , fontSizeCls fontSize
            ]
    , children: children
    }

p_ :: Array JSX -> JSX
p_ children = p pProps { children = children }

type SpanProps
  = { children :: Array JSX
    , bold :: Boolean
    , fontSize :: FontSize
    }

spanProps =
  { children: mempty
  , bold: false
  , fontSize: Medium
  } ::
    SpanProps

span :: SpanProps -> JSX
span { bold, fontSize, children } =
  DOM.span
    { className:
        intercalate " "
          $ pure styles.span
          <> pure (fontSizeCls fontSize)
          <> guard bold [ "bold" ]
    , children
    }

span_ :: Array JSX -> JSX
span_ children = span spanProps { children = children }

mark :: Array JSX -> JSX
mark children =
  DOM.mark
    { className: styles.mark
    , children
    }

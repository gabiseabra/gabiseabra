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
  , a
  , autoLink
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array (mapWithIndex)
import Data.Either (Either, either)
import Data.Filterable (maybeBool)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid as Monoid
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX, fragment)

foreign import styles :: Styles

type Styles
  = { heading :: String
    , paragraph :: String
    , span :: String
    , mark :: String
    , link :: String
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
          <> Monoid.guard bold [ "bold" ]
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

type LinkProps
  = { children :: Array JSX
    , href :: String
    }

aProps =
  { children: mempty
  , href: "#"
  } ::
    LinkProps

a :: LinkProps -> JSX
a { href, children } =
  DOM.a
    { className: styles.link
    , href
    , children
    }

data Link
  = URL String
  | GH String String

mkLink :: String -> Maybe Link
mkLink text = parseURL <|> parseGH
  where
  slash = String.Pattern "/"

  parseURL = maybeBool isURL text <#> URL

  parseGH =
    String.indexOf slash text
      >>= (flip String.splitAt) text
      >>> \{ before, after } -> pure $ GH before $ String.drop 1 after

isURL :: String -> Boolean
isURL text = startsWith http text || startsWith https text
  where
  startsWith :: String.Pattern -> String -> Boolean
  startsWith pat x = String.indexOf pat x == Just 0

  http = String.Pattern "http://"

  https = String.Pattern "https://"

link :: Link -> JSX
link (URL href) = a { href, children: pure $ DOM.text href }

link (GH user repo) =
  a
    { href: "https://" <> user <> ".github.io/" <> repo
    , children: pure $ DOM.text $ user <> "/" <> repo
    }

autoLink :: String -> JSX
autoLink text =
  regex
    # either fail
        ( (flip Regex.split) text
            >>> mapWithIndex render
            >>> fragment
        )
  where
  fail err = DOM.text $ "[AutoLink Error] " <> err

  regex :: Either String Regex
  regex =
    let
      url = "https?\\:\\/\\/.*(?:\\.\\w+)+"

      gh = "[\\w\\d-_]+\\/[\\w\\d-_]+"
    in
      Regex.regex ("(" <> url <> "|" <> gh <> ")") mempty

  render :: Int -> String -> JSX
  render i
    | mod i 2 == 0 = DOM.text
    | otherwise = mkLink >>> maybe (fail $ "invalid link type") link

module Hey.Components.SVG.Icon
  ( Icon(..)
  , icon
  ) where

import Prelude
import React.Basic (JSX)
import React.Basic.DOM.SVG as SVG
import Data.Tuple.Nested (type (/\), (/\))

data Icon
  = Clock
  | Github
  | ExternalLink
  | Code
  | CodeBranch
  | Star
  | HandsHelping
  | Nag

derive instance eqIcon :: Eq Icon

type IconDefinition
  = { path :: String
    , width :: Number
    , height :: Number
    }

foreign import icons ::
  { clock :: IconDefinition
  , github :: IconDefinition
  , externalLink :: IconDefinition
  , code :: IconDefinition
  , codeBranch :: IconDefinition
  , star :: IconDefinition
  , handsHelping :: IconDefinition
  , nag :: IconDefinition
  , empty :: IconDefinition
  }

def :: Icon -> String /\ IconDefinition
def ic
  | ic == Clock = "clock" /\ icons.clock
  | ic == Github = "github" /\ icons.github
  | ic == ExternalLink = "externalLink" /\ icons.externalLink
  | ic == Code = "code" /\ icons.code
  | ic == CodeBranch = "codeBranch" /\ icons.codeBranch
  | ic == Star = "star" /\ icons.star
  | ic == HandsHelping = "handsHelping" /\ icons.handsHelping
  | ic == Nag = "nag" /\ icons.nag
  | otherwise = "" /\ icons.empty

icon :: Icon -> JSX
icon =
  def
    >>> \(className /\ { path, width, height }) ->
        SVG.svg
          { className: "icon " <> className
          , viewBox: "0 0 " <> (show width) <> " " <> (show height)
          , x: "0"
          , y: "0"
          , children:
              pure
                $ SVG.path { d: path }
          }

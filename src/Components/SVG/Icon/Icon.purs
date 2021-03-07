module Hey.Components.SVG.Icon
  ( Icon(..)
  , icon
  ) where

import Prelude
import React.Basic (JSX)
import React.Basic.DOM.SVG as SVG

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

def :: Icon -> IconDefinition
def ic
  | ic == Clock = icons.clock
  | ic == Github = icons.github
  | ic == ExternalLink = icons.externalLink
  | ic == Code = icons.code
  | ic == CodeBranch = icons.codeBranch
  | ic == Star = icons.star
  | ic == HandsHelping = icons.handsHelping
  | ic == Nag = icons.nag
  | otherwise = icons.empty

icon :: Icon -> JSX
icon =
  def
    >>> \{ path, width, height } ->
        SVG.svg
          { className: "icon"
          , viewBox: "0 0 " <> (show width) <> " " <> (show height)
          , x: "0"
          , y: "0"
          , children:
              pure
                $ SVG.path { d: path }
          }

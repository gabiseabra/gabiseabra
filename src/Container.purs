module Container where

import React.Basic (JSX)
import React.Basic.DOM as DOM

container :: JSX
container =
  DOM.div
    { children: [ DOM.text "eyy", DOM.text "lmao" ]
    }
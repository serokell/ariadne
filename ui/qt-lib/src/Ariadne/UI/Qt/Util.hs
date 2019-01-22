module Ariadne.UI.Qt.Util
  ( formatBalance
  , unitToHtml
  , unitToHtmlSmall
  ) where

import Ariadne.UI.Qt.Face

formatBalance :: Text -> Currency -> Text
formatBalance balance unit = balance <> " " <> unitToHtml unit

unitToHtml :: Currency -> Text
unitToHtml ADA = "<img src=':/images/ada-symbol-big-dark.png'>"
unitToHtml Lovelace = "Lovelace"

unitToHtmlSmall :: Currency -> Text
unitToHtmlSmall ADA = "<img src=':/images/ada-symbol-small-dark.png'>"
unitToHtmlSmall Lovelace = "Lovelace"

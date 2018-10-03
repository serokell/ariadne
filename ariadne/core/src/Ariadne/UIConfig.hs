module Ariadne.UIConfig
  ( licenseUrl
  , changelogUrl
  , aboutUrl
  ) where

import Data.Text(Text)

licenseUrl :: Text
licenseUrl = "https://serokell.io/ariadne/license"

changelogUrl :: Text
changelogUrl = "https://github.com/serokell/ariadne/blob/master/CHANGELOG.md"

aboutUrl :: Text
aboutUrl = "https://github.com/serokell/ariadne"

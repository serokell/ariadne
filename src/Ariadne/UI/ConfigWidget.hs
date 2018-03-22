module Ariadne.UI.ConfigWidget where

import Prelude
import Data.Text (Text)
import Data.Void
import Control.Lens
import Control.Monad.Trans.Writer

import qualified Brick as B
import qualified Brick.Forms as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Themes as B
import qualified Graphics.Vty as V

import Ariadne.Util

-- User information that can be configured using this widget.
data UserInfo =
  UserInfo
    { name     :: Text
    , email    :: Text
    , language :: Text
    , city     :: Text
    , theme    :: B.Theme
    } deriving (Show)

makeLensesWith postfixLFields ''UserInfo

defaultUserInfo :: UserInfo
defaultUserInfo =
  UserInfo
    { name  = ""
    , email = ""
    , language = ""
    , city  = ""
    , theme = defaultTheme
    }

-- The state of the configuration form.
type ConfigWidgetState =
  B.Form UserInfo -- The user is entering 'UserInfo' into the form.

-- Identifiers for the parts of the form.
data ConfigWidgetSelector
  = NameField
  | EmailField
  | LanguageField
  | CityField
  | DefaultThemeField
  | Theme1Field
  | HandedField
  | FormViewport
  deriving (Eq, Ord, Show)

-- Create a new form from the initial user info.
initConfigWidget
  :: (Ord name, Show name, HasReview name ConfigWidgetSelector)
  => UserInfo
  -> ConfigWidgetState ev name
initConfigWidget =
    let
      label s w =
        B.padBottom (B.Pad 1) $
          (B.vLimit 1 $ B.hLimit 15 $ B.str s B.<+> B.fill ' ') B.<+> w
    in
      B.newForm
        [ label "Name" B.@@= B.editTextField nameL (inj NameField) (Just 1)
        , label "Email" B.@@= B.editTextField emailL (inj EmailField) (Just 1)
        , label "Language" B.@@= B.editTextField languageL (inj LanguageField) (Just 1)
        , label "City/Town" B.@@= B.editTextField cityL (inj CityField) (Just 1)
        , label "Theme" B.@@=
            B.radioField themeL
              [ (defaultTheme, inj DefaultThemeField, "Default")
              , (theme1, inj Theme1Field, "White on Blue")
              ]
        ]

-- The default color theme.
defaultTheme :: B.Theme
defaultTheme = B.newTheme V.defAttr
  [ (B.editAttr, V.white `B.on` V.black)
  , (B.editFocusedAttr, V.black `B.on` V.yellow)
  , (B.invalidFormInputAttr, V.white `B.on` V.red)
  , (B.focusedFormInputAttr, V.black `B.on` V.yellow)
  ]

-- An alternative color theme.
theme1 :: B.Theme
theme1 = B.newTheme (V.white `B.on` V.blue)
  [ (B.editAttr, V.white `B.on` V.black)
  , (B.editFocusedAttr, V.black `B.on` V.yellow)
  , (B.invalidFormInputAttr, V.white `B.on` V.red)
  , (B.focusedFormInputAttr, V.black `B.on` V.yellow)
  ]

drawConfigWidget
  :: (Ord name, Show name, HasReview name ConfigWidgetSelector)
  => ConfigWidgetState ev name
  -> B.Widget name
drawConfigWidget configWidgetState =
    B.viewport (inj FormViewport) B.Vertical (B.hCenter form) B.<=>
    B.hCenter help
  where
    form =
      B.borderWithLabel (B.str "Configuration") $
      B.padTop (B.Pad 1) $
      B.hLimit 80 $
      B.renderForm configWidgetState
    help =
      B.borderWithLabel (B.str "Help") $ B.str
        "- Use the mouse to select fields\n\
        \- Type in your configuration settings\n\
        \- Tab/Shift-Tab also selects fields\n\
        \- Scroll the form with Up/Down\n\
        \- Enter/Esc quit"

data ConfigCompleted = ConfigCompleted | ConfigInProgress

handleConfigWidgetEvent
  :: (Ord name, Show name, HasReview name ConfigWidgetSelector)
  => B.BrickEvent name ev
  -> ConfigWidgetState ev name
  -> WriterT ConfigCompleted (B.EventM name) (ConfigWidgetState ev name)
handleConfigWidgetEvent ev configWidgetState =
  WriterT $ case ev of
    B.VtyEvent (V.EvKey V.KEsc []) ->
      return (configWidgetState, ConfigCompleted)
    B.VtyEvent (V.EvKey V.KEnter []) ->
      return (configWidgetState, ConfigCompleted)
    B.VtyEvent (V.EvKey V.KDown []) ->
      (configWidgetState, ConfigInProgress) <$ vFormScroll 1
    B.VtyEvent (V.EvKey V.KUp []) ->
      (configWidgetState, ConfigInProgress) <$ vFormScroll (-1)
    _ -> (, ConfigInProgress) <$> B.handleFormEvent ev configWidgetState
  where
    vFormScroll = B.vScrollBy (B.viewportScroll (inj FormViewport))

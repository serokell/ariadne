{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ariadne.ConfigForm where

import qualified Data.Text as T
import qualified Text.RawString.QQ as QQ
import Lens.Micro ((^.))
import Lens.Micro.TH
import Data.Monoid ((<>))


import qualified Graphics.Vty as V
import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

data Name = NameField
          | EmailField
          | LanguageField
          | CityField
          deriving (Eq, Ord, Show)

data UserInfo =
    UserInfo { _name      :: T.Text
             , _email     :: T.Text
             , _language     :: T.Text
             , _city      :: T.Text
             }
             deriving (Show)

defaultUserInfo :: UserInfo
defaultUserInfo =
  UserInfo { _name  = ""
           , _email = ""
           , _language = ""
           , _city  = ""
           }

makeLenses ''UserInfo

mkForm :: UserInfo -> Form UserInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Name" @@=
                   editTextField name NameField (Just 1)
               , label "Email" @@=
                   editTextField email EmailField (Just 1)
               , label "Language" @@=
                   editTextField language LanguageField (Just 1)
               , label "City/Town" @@=
                   editTextField city CityField (Just 1)
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form UserInfo e Name -> [Widget Name]
draw f = [ (C.vCenter $ C.hCenter $ txt ariadneBanner) <=>
           (C.hCenter form) <=>
           (C.vCenter $ C.hCenter help)
         ]
    where
        form = B.borderWithLabel (str "Configuration") $ padTop (Pad 1) $
                  hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Fields ar are-form text\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"
ariadneMaze :: T.Text
ariadneMaze = [QQ.r|
      #\                           /#
      ##\                         /##
      ###\                       /###
      ####]                     [####
      ####]                     [####
      ####]___               ___[####
      ####]__]\             /[__[####
      ####]__]#\           /#[__[####
      ####]__]##]         [##[__[####
      ####]__]##]__     __[##[__[####
      ####]__]##]_]\___/[_[##[__[####
      ####]__]##]_]#|_|#[_[##[__[####
      ####]__]##]_]/   \[_[##[__[####
      ####]__]##]---------[##[__[####
      ####]__]#/           \#[__[####
      ####]__]/             \[__[####
      ####]                     [####
      ####]=====================[####
      ####]                     [####
      ###/                       \###
      ##/                         \##
      #/                           \#
|]

ariadneBanner :: T.Text
ariadneBanner = [QQ.r|
  ___  ____ ___  ___           _   _ _____
 / _ \|  _ (   )/ _ \    /\   | \ | |  ___)
| |_| | |_) ) || |_| |  /  \  |  \| | |_
|  _  |    /| ||  _  | / /\ \ |     |  _)
| | | | |\ \| || | | |/ /__\ \| |\  | |___
|_| |_|_| \(___)_| |_/________\_| \_|_____)
|]

app :: App (Form UserInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent (formFocus s) /= Just EmailField -> halt s
                _ -> do
                    s' <- handleFormEvent ev s
                    continue s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        f = mkForm defaultUserInfo

    f' <- customMain buildVty Nothing app f

    putStrLn "The starting form state was:"
    print defaultUserInfo

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')

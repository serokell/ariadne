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
import Graphics.Vty
  ( Color, black, red, green, yellow, blue, magenta, cyan, white
  , brightBlack, brightRed, brightGreen, brightYellow
  , brightBlue, brightMagenta, brightCyan, brightWhite
  )
    
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
import qualified Brick.Main           as Main
import qualified Brick.Widgets.Edit   as Edit
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Center as Center
import qualified Brick.Types          as Types
import qualified Brick.Themes         as Themes

import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , str
  )

data Name = NameField
          | EmailField
          | LanguageField
          | CityField
          | FGColorField
          | FGBlackField
          | FGRedField
          | FGGreenField
          | FGYellowField
          | FGBlueField
          | FGMagentaField
          | FGCyanField
          | FGWhiteField
          | BGColorField
          | BGBlackField
          | BGRedField
          | BGGreenField
          | BGYellowField
          | BGBlueField
          | BGMagentaField
          | BGCyanField
          | BGWhiteField
          | Acc1ColorField
          | Acc1BlackField
          | Acc1RedField
          | Acc1GreenField
          | Acc1YellowField
          | Acc1BlueField
          | Acc1MagentaField
          | Acc1CyanField
          | Acc1WhiteField
          | Acc2ColorField
          | Acc2BlackField
          | Acc2RedField
          | Acc2GreenField
          | Acc2YellowField
          | Acc2BlueField
          | Acc2MagentaField
          | Acc2CyanField
          | Acc2WhiteField
          | HandedField
          | FormViewport
          deriving (Eq, Ord, Show)

data UserInfo =
    UserInfo { _name      :: T.Text
             , _email     :: T.Text
             , _language  :: T.Text
             , _city      :: T.Text
             , _fgColor   :: Color
             , _bgColor   :: Color
             , _acc1Color :: Color
             , _acc2Color :: Color
             }
             deriving (Show)

defaultUserInfo :: UserInfo
defaultUserInfo = 
  UserInfo { _name  = ""
           , _email = ""
           , _language = ""
           , _city  = ""
           , _fgColor = black
           , _bgColor = white
           , _acc1Color = yellow
           , _acc2Color = red
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
               , label "FG Color" @@=
                   radioField fgColor 
                    [ (black, FGBlackField, "black")
                    , (red, FGRedField, "red")
                    , (yellow, FGYellowField, "yellow")
                    , (blue, FGBlueField, "blue")
                    , (magenta, FGMagentaField, "magenta")
                    , (cyan, FGCyanField, "cyan")
                    , (white, FGWhiteField, "white")
                    ]
                , label "BG Color" @@= radioField bgColor 
                      [ (black, BGBlackField, "black")
                      , (red, BGRedField, "red")
                      , (yellow, BGYellowField, "yellow")
                      , (blue, BGBlueField, "blue")
                      , (magenta, BGMagentaField, "magenta")
                      , (cyan, BGCyanField, "cyan")
                      , (white, BGWhiteField, "white")
                      ]
                , label "Accent 1 Color" @@= radioField acc1Color 
                      [ (black, Acc1BlackField, "black")
                      , (red, Acc1RedField, "red")
                      , (yellow, Acc1YellowField, "yellow")
                      , (blue, Acc1BlueField, "blue")
                      , (magenta, Acc1MagentaField, "magenta")
                      , (cyan, Acc1CyanField, "cyan")
                      , (white, Acc1WhiteField, "white")
                      ]
                , label "Accent 2 Color" @@= radioField acc2Color 
                      [ (black, Acc2BlackField, "black")
                      , (red, Acc2RedField, "red")
                      , (yellow, Acc2YellowField, "yellow")
                      , (blue, Acc2BlueField, "blue")
                      , (magenta, Acc2MagentaField, "magenta")
                      , (cyan, Acc2CyanField, "cyan")
                      , (white, Acc2WhiteField, "white")
                      ]
               ]

theMap :: Form UserInfo e Name -> AttrMap
theMap form = 
  attrMap (fg `on` bg)
  [ (Edit.editAttr, bg `on` fg)
  , (Edit.editFocusedAttr, bg `on` acc2)
  , (invalidFormInputAttr, bg `on` acc2)
  , (focusedFormInputAttr, fg `on` acc2)
  ]
  where
    fs   = formState form
    fg   = _fgColor fs
    bg   = _bgColor fs
    acc1 = _acc1Color fs 
    acc2 = _acc2Color fs 

draw :: Form UserInfo e Name -> [Widget Name]
draw f = [ (viewport FormViewport Vertical $ Center.hCenter $ form) <=>
           (Center.hCenter help) 
         ]
    where
        form = Border.borderWithLabel (str "Configuration") $ padTop (Pad 1) $
                  hLimit 80 $ renderForm f
        help = Border.borderWithLabel (str "Help") body
        body = str $ "- Use the mouse to select fields\n" <>
                     "- Type in your configuration settings\n" <>
                     "- Tab/Shift-Tab also selects fields\n" <>
                     "- Scroll the form with Up/Down\n" <>
                     "- Enter/Esc quit"

app :: App (Form UserInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = appEvent
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = theMap
        }

formScroll :: Main.ViewportScroll Name
formScroll = Main.viewportScroll FormViewport

appEvent :: Form UserInfo e Name 
         -> Types.BrickEvent Name e 
         -> Types.EventM Name (Types.Next (Form UserInfo e Name))
appEvent s ev =
  case ev of
    VtyEvent (V.EvResize {})       -> continue s
    VtyEvent (V.EvKey V.KEsc [])   -> halt s
    VtyEvent (V.EvKey V.KEnter []) -> halt s 
    VtyEvent (V.EvKey V.KDown [])  -> vScrollBy formScroll 1 >> continue s
    VtyEvent (V.EvKey V.KUp [])    -> vScrollBy formScroll (-1) >> continue s
    _ -> do
        s' <- handleFormEvent ev s
        continue s'

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

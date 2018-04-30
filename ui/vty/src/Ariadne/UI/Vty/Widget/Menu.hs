module Ariadne.UI.Vty.Widget.Menu where

import Universum

import Control.Lens (assign, makeLensesWith, uses, zoom, (.=))
import Control.Monad.Trans.State.Strict as State (modify)
import Data.Char (toLower)
import Data.Function (fix)
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Vector as Vector

import qualified Brick as B
import qualified Brick.Widgets.Center as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Keyboard

import IiExtras

data MenuWidgetState a n =
  MenuWidgetState
    { menuWidgetElems :: Vector (MenuWidgetElem a)
    , menuWidgetSelection :: Int -- invariant: (`mod` length xs)
    , menuWidgetNavMode :: Bool
    , menuWidgetBrickName :: n
    }

data MenuWidgetElem a =
  MenuWidgetElem
    { menuWidgetElemSelector :: a
    , menuWidgetElemText :: Text
    , menuWidgetElemKey :: Char
    }

makeLensesWith postfixLFields ''MenuWidgetState
makeLensesWith postfixLFields ''MenuWidgetElem

menuWidgetSel :: MenuWidgetState a n -> a
menuWidgetSel MenuWidgetState{..} =
  -- the lookup is safe due to the invariant on 'menuWidgetSelection'
  menuWidgetElemSelector $ menuWidgetElems Vector.! menuWidgetSelection

menuWidgetCharToSel :: Char -> MenuWidgetState a n -> Maybe a
menuWidgetCharToSel key MenuWidgetState{..} =
  view menuWidgetElemSelectorL <$> Vector.find ((== toLower key) . menuWidgetElemKey) menuWidgetElems

initMenuWidget :: NonEmpty (MenuWidgetElem a) -> Int -> n -> MenuWidgetState a n
initMenuWidget xs i name =
  fix $ \this -> MenuWidgetState
    { menuWidgetElems = Vector.fromList (NonEmpty.toList xs)
    , menuWidgetSelection = i `mod` Vector.length (menuWidgetElems this)
    , menuWidgetNavMode = False
    , menuWidgetBrickName = name
    }

drawMenuWidget
  :: MenuWidgetState a n
  -> B.Widget n
drawMenuWidget MenuWidgetState{..} =
  B.withAttr "menu" $
    B.hCenter $
    B.clickable menuWidgetBrickName B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    render = do
      rdrCtx <- B.getContext
      let
        defAttr = rdrCtx ^. B.attrL
        attrMap = rdrCtx ^. B.ctxAttrMapL

        menuElems = Vector.toList menuWidgetElems
        i = menuWidgetSelection

        drawElem j menuElem = V.horizCat
          [ V.text' elemAttr $ " " <> beforeKey
          , V.text' keyAttr $ T.singleton key
          , V.text' elemAttr $ afterKey <> " "
          ]
          where
            elemAttr = if i == j
              then defAttr <> B.attrMapLookup "menu.selected" attrMap
              else defAttr
            keyAttr = if menuWidgetNavMode
              then elemAttr <> B.attrMapLookup "menu.key" attrMap
              else elemAttr
            elemText = menuWidgetElemText menuElem
            elemKey = menuWidgetElemKey menuElem
            (beforeKey, atKey) = T.break ((== elemKey) . toLower) elemText
            (key, afterKey)
              | T.null atKey = (elemKey, atKey)
              | toLower (T.head atKey) == elemKey = (T.head atKey, T.tail atKey)
              | otherwise = (elemKey, atKey)

        img =
          V.horizCat $
          List.intersperse (V.text' defAttr " ") $
          List.zipWith drawElem [0..] menuElems

      return $
        B.emptyResult
          & B.imageL .~ img

data MenuWidgetEvent a
  = MenuNextEvent
  | MenuPrevEvent
  | MenuSelectEvent (a -> Bool)
  | MenuEnterEvent
  | MenuExitEvent
  | MenuMouseDownEvent V.Button [V.Modifier] B.Location

keyToMenuWidgetEvent
  :: Eq a
  => MenuWidgetState a n
  -> KeyboardEvent
  -> Maybe (MenuWidgetEvent a)
keyToMenuWidgetEvent menuWidgetState = \case
  KeyNavigation -> Just MenuExitEvent
  KeyEnter -> Just MenuExitEvent
  KeyChar ' ' -> Just MenuExitEvent
  KeyChar c
    | Just sel <- menuWidgetCharToSel c menuWidgetState -> Just $ MenuSelectEvent (== sel)
    | otherwise -> Just MenuEnterEvent -- Stay in menu
  KeyLeft -> Just MenuPrevEvent
  KeyRight -> Just MenuNextEvent
  _ -> Nothing -- Exit menu and leave key processing to other widgets

handleMenuWidgetEvent
  :: MenuWidgetEvent a
  -> StateT (MenuWidgetState a n) (B.EventM n) ()
handleMenuWidgetEvent ev = do
  len <- uses menuWidgetElemsL Vector.length
  let
    modifySelection f =
      zoom menuWidgetSelectionL $ State.modify $ (`mod` len) . f
    colToSelection :: Vector (MenuWidgetElem a) -> Int -> Maybe Int
    colToSelection = go 0
      where
        go acc elems col
          | Vector.null elems = Nothing
          | col < 1 = Nothing -- 1-char space between items
          | col <= w = Just acc
          | otherwise = go (acc + 1) (Vector.tail elems) (col - w - 1)
          where
            el = Vector.head elems
            -- Width of menu item, including 1-char padding on both sides
            w = 2 + T.length (menuWidgetElemText el)
  case ev of
    MenuNextEvent -> modifySelection succ
    MenuPrevEvent -> modifySelection pred
    MenuEnterEvent -> menuWidgetNavModeL .= True
    MenuExitEvent -> menuWidgetNavModeL .= False
    MenuSelectEvent p -> do
      mI <- uses menuWidgetElemsL (Vector.findIndex (p . menuWidgetElemSelector))
      whenJust mI (assign menuWidgetSelectionL)
      menuWidgetNavModeL .= False
    MenuMouseDownEvent V.BLeft [] (B.Location (col, _)) -> do
      elems <- use menuWidgetElemsL
      whenJust (colToSelection elems col) (assign menuWidgetSelectionL)
    MenuMouseDownEvent _ _ _ ->
      return ()

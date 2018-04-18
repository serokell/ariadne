module Ariadne.UI.Vty.Widget.Menu where

import Control.Lens
import Control.Monad.Trans.State as State
import Data.Char
import Data.Foldable
import Data.Function (fix)
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector as Vector
import Prelude

import qualified Brick as B
import qualified Brick.Widgets.Center as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Keyboard

import IiExtras

data MenuWidgetState a =
  MenuWidgetState
    { menuWidgetElems :: Vector (MenuWidgetElem a)
    , menuWidgetSelection :: Int -- invariant: (`mod` length xs)
    , menuWidgetNavMode :: Bool
    }

data MenuWidgetElem a =
  MenuWidgetElem
    { menuWidgetElemSelector :: a
    , menuWidgetElemText :: Text
    , menuWidgetElemKey :: Char
    }

makeLensesWith postfixLFields ''MenuWidgetState
makeLensesWith postfixLFields ''MenuWidgetElem

menuWidgetSel :: MenuWidgetState a -> a
menuWidgetSel MenuWidgetState{..} =
  -- the lookup is safe due to the invariant on 'menuWidgetSelection'
  menuWidgetElemSelector $ menuWidgetElems Vector.! menuWidgetSelection

menuWidgetCharToSel :: Char -> MenuWidgetState a -> Maybe a
menuWidgetCharToSel key MenuWidgetState{..} =
  view menuWidgetElemSelectorL <$> Vector.find ((== toLower key) . menuWidgetElemKey) menuWidgetElems

initMenuWidget :: NonEmpty (MenuWidgetElem a) -> Int -> MenuWidgetState a
initMenuWidget xs i =
  fix $ \this -> MenuWidgetState
    { menuWidgetElems = Vector.fromList (NonEmpty.toList xs)
    , menuWidgetSelection = i `mod` Vector.length (menuWidgetElems this)
    , menuWidgetNavMode = False
    }

drawMenuWidget
  :: MenuWidgetState a
  -> B.Widget name
drawMenuWidget menuWidgetState =
  B.withAttr "menu" $ B.hCenter B.Widget
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

        menuElems = Vector.toList (menuWidgetElems menuWidgetState)
        i = menuWidgetSelection menuWidgetState

        drawElem j menuElem = V.horizCat
          [ V.text' elemAttr $ " " <> beforeKey
          , V.text' keyAttr $ T.singleton key
          , V.text' elemAttr $ afterKey <> " "
          ]
          where
            elemAttr = if i == j
              then defAttr <> B.attrMapLookup "menu.selected" attrMap
              else defAttr
            keyAttr = if menuWidgetState ^. menuWidgetNavModeL
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

keyToMenuWidgetEvent
  :: Eq a
  => MenuWidgetState a
  -> KeyboardEvent
  -> Maybe (MenuWidgetEvent a)
keyToMenuWidgetEvent menuWidgetState = \case
  KeyNavigation -> Just MenuExitEvent
  KeyEnter -> Just MenuExitEvent
  KeyChar c
    | Just sel <- menuWidgetCharToSel c menuWidgetState -> Just $ MenuSelectEvent (== sel)
    | otherwise -> Just MenuEnterEvent -- Stay in menu
  KeyLeft -> Just MenuPrevEvent
  KeyRight -> Just MenuNextEvent
  _ -> Nothing -- Exit menu and leave key processing to other widgets

handleMenuWidgetEvent
  :: MenuWidgetEvent a
  -> StateT (MenuWidgetState a) (B.EventM n) ()
handleMenuWidgetEvent ev = do
  len <- uses menuWidgetElemsL Vector.length
  let
    modifySelection f =
      zoom menuWidgetSelectionL $ State.modify $ (`mod` len) . f
  case ev of
    MenuNextEvent -> modifySelection succ
    MenuPrevEvent -> modifySelection pred
    MenuEnterEvent -> menuWidgetNavModeL .= True
    MenuExitEvent -> menuWidgetNavModeL .= False
    MenuSelectEvent p -> do
      mI <- uses menuWidgetElemsL (Vector.findIndex (p . menuWidgetElemSelector))
      for_ mI (menuWidgetSelectionL .=)
      menuWidgetNavModeL .= False

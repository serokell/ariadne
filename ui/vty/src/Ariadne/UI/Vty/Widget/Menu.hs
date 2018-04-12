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

import IiExtras

data MenuWidgetState a =
  MenuWidgetState
    { menuWidgetElems :: Vector (MenuWidgetElem a)
    , menuWidgetSelection :: Int -- invariant: (`mod` length xs)
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
  view menuWidgetElemSelectorL <$> Vector.find ((== key) . menuWidgetElemKey) menuWidgetElems


initMenuWidget :: NonEmpty (MenuWidgetElem a) -> Int -> MenuWidgetState a
initMenuWidget xs i =
  fix $ \this -> MenuWidgetState
    { menuWidgetElems = Vector.fromList (NonEmpty.toList xs)
    , menuWidgetSelection = i `mod` Vector.length (menuWidgetElems this)
    }

drawMenuWidget
  :: Bool
  -> MenuWidgetState a
  -> B.Widget name
drawMenuWidget appStateNavigationMode menuWidgetState =
  B.withAttr "menu" $ B.hCenter $ B.hBox $ List.intersperse (B.txt " ") elemWidgets
  where
    menuElems = Vector.toList (menuWidgetElems menuWidgetState)
    i = menuWidgetSelection menuWidgetState

    elemWidgets = List.zipWith drawElem [0..] menuElems

    drawElem j menuElem = B.withAttr attr drawText
      where
        attr = if i == j then "menu" <> "selected" else "menu"
        keyAttr = if appStateNavigationMode then attr <> "key" else attr

        elemText = menuWidgetElemText menuElem
        elemKey = menuWidgetElemKey menuElem

        (beforeKey, atKey) = T.break ((== elemKey) . toLower) elemText
        (key, afterKey)
          | T.null atKey = (elemKey, atKey)
          | toLower (T.head atKey) == elemKey = (T.head atKey, T.tail atKey)
          | otherwise = (elemKey, atKey)

        drawText = B.hBox
          [ B.txt $ " " <> beforeKey
          , B.withAttr keyAttr $ B.txt $ T.singleton key
          , B.txt $ afterKey <> " "
          ]

data MenuWidgetEvent a
  = MenuNextEvent
  | MenuPrevEvent
  | MenuSelectEvent (a -> Bool)

handleMenuWidgetEvent
  :: MenuWidgetEvent a
  -> StateT (MenuWidgetState a) IO ()
handleMenuWidgetEvent ev = do
  len <- uses menuWidgetElemsL Vector.length
  let
    modifySelection f =
      zoom menuWidgetSelectionL $ State.modify $ (`mod` len) . f
  case ev of
    MenuNextEvent -> modifySelection succ
    MenuPrevEvent -> modifySelection pred
    MenuSelectEvent p -> do
      mI <- uses menuWidgetElemsL (Vector.findIndex (p . menuWidgetElemSelector))
      for_ mI (menuWidgetSelectionL .=)

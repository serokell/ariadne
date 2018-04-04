module Ariadne.UI.Vty.Widget.Menu where

import Control.Lens
import Control.Monad.Trans.State as State
import Data.Foldable
import Data.Function (fix)
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Vector as Vector
import Prelude

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import IiExtras

data MenuWidgetState a =
  MenuWidgetState
    { menuWidgetElems :: Vector a
    , menuWidgetSelection :: Int -- invariant: (`mod` length xs)
    }

makeLensesWith postfixLFields ''MenuWidgetState

menuWidgetSel :: MenuWidgetState a -> a
menuWidgetSel MenuWidgetState{..} =
  -- the lookup is safe due to the invariant on 'menuWidgetSelection'
  menuWidgetElems Vector.! menuWidgetSelection

initMenuWidget :: NonEmpty a -> Int -> MenuWidgetState a
initMenuWidget xs i =
  fix $ \this -> MenuWidgetState
    { menuWidgetElems = Vector.fromList (NonEmpty.toList xs)
    , menuWidgetSelection = i `mod` Vector.length (menuWidgetElems this)
    }

drawMenuWidget
  :: Bool
  -> (a -> Text)
  -> MenuWidgetState a
  -> B.Widget name
drawMenuWidget appStateNavigationMode textElem menuWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        (leftPad, rightPad) =
          integralDistribExcess
            (rdrCtx ^. B.availWidthL)
            (V.imageWidth img)

        menuElems = Vector.toList (menuWidgetElems menuWidgetState)
        i = menuWidgetSelection menuWidgetState

        drawElemNavMode _ x =
          let
            selectorText = textElem x
            firstLetter
              | T.null selectorText = error "Bug: empty title"
              | otherwise = (T.singleton . T.head) selectorText
            titleList = [firstLetter, " - ", selectorText]
          in
            V.horizCat $ List.map (V.text' backMenuAttr) titleList

        drawElemSelectMode j x =
          let
            attr
              | i == j =
                V.defAttr
                  `V.withForeColor` V.white
                  `V.withBackColor` V.black
              | otherwise = backMenuAttr
          in
            V.text' attr (textElem x)

        backMenuAttr =
          V.defAttr
            `V.withForeColor` V.black
            `V.withBackColor` V.white

        fill n = V.charFill @Int backMenuAttr ' ' n 1

        drawElem = if appStateNavigationMode
          then drawElemNavMode
          else drawElemSelectMode

        img =
          V.horizCat $
          List.intersperse (fill 3) $
          List.zipWith drawElem [0..] menuElems

        img' =
          V.horizCat [fill leftPad, img, fill rightPad]

      return $
        B.emptyResult
          & B.imageL .~ img'

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
      mI <- uses menuWidgetElemsL (Vector.findIndex p)
      for_ mI (menuWidgetSelectionL .=)

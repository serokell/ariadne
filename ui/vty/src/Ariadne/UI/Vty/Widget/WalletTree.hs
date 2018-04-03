-- | Wallet tree widget and its data model.

module Ariadne.UI.Vty.Widget.WalletTree
       ( WalletTreeWidgetState
       , initWalletTreeWidget
       , drawWalletTreeWidget
       , WalletTreeWidgetEvent(..)
       , handleWalletTreeWidgetEvent
       ) where

import Universum hiding (StateT, (.~))

import Control.Lens
import Control.Monad.Trans.State
import Data.List (intersperse)
import Data.Tree (Tree(..))
import IiExtras
import Serokell.Util (enumerate)

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face

----------------------------------------------------------------------------
-- General (should probably be moved somewhere at later stage)
----------------------------------------------------------------------------

-- | Whether an item is selected.
data SelectionFlag
    = NotSelected
    | Selected

renderTree ::
       forall a.
       Maybe TreePath
    -> (SelectionFlag -> TreePath -> a -> V.Image)
    -> Tree a
    -> V.Image
renderTree selection toImg = go [] []
  where
    map' :: (from -> Bool -> to) -> [from] -> [to]
    map' _ [] = []
    map' f [x] = [f x True]
    map' f (x:xs) = f x False : map' f xs
    prefixPart :: Bool -> Bool -> Text
    prefixPart True  False = "│   "
    prefixPart True  True  = "├── "
    prefixPart False False = "    "
    prefixPart False True  = "└── "
    prefix :: [Bool] -> V.Image
    prefix prefixLines = V.text' V.defAttr $ mconcat $ map' prefixPart prefixLines
    selectionFlag :: TreePath -> SelectionFlag
    selectionFlag curPath
        | Just curPath == selection = Selected
        | otherwise = NotSelected
    go :: TreePath -> [Bool] -> Tree a -> V.Image
    go curPath prefixLines Node {..} =
        V.vertCat
        ( V.horizJoin (prefix prefixLines)
                      (toImg (selectionFlag curPath) curPath rootLabel)
        : map' (\(i, child) isLast -> go (curPath ++ [i]) (prefixLines ++ [not isLast]) child) (enumerate subForest)
        )

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

-- | State of wallet tree widget, basically the data we want to
-- display (corresponds to a list of wallets).
data WalletTreeWidgetState =
  WalletTreeWidgetState
    { walletTreeWallets :: ![UiWalletTree]
    , walletTreeSelection :: !(Maybe UiWalletTreeSelection)
    }

makeLensesWith postfixLFields ''WalletTreeWidgetState

initWalletTreeWidget :: WalletTreeWidgetState
initWalletTreeWidget = WalletTreeWidgetState [] (Just (UiWalletTreeSelection 0 [0]))

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

renderWalletTreeItem :: SelectionFlag -> TreePath -> UiWalletTreeItem -> V.Image
renderWalletTreeItem selection _ UiWalletTreeItem {..} = V.text' attr toDisplay
  where
    toDisplay =
        case wtiLabel of
            Nothing
                | wtiShowPath -> pathText
                | otherwise -> "★"
            Just label
                | wtiShowPath -> label <> " (" <> pathText <> ")"
                | otherwise -> label
    attr =
        case selection of
            NotSelected -> V.defAttr
            Selected -> selAttr
    selAttr = V.defAttr `V.withForeColor` V.black `V.withBackColor` V.white
    pathText = T.intercalate "-" $ map (pretty . succ) wtiPath

drawWalletTreeWidget
  :: Bool
  -> WalletTreeWidgetState
  -> B.Widget name
drawWalletTreeWidget _hasFocus (WalletTreeWidgetState wallets mSelection) =
  B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    render = do
      let
        renderOneTree :: (Word, UiWalletTree) -> V.Image
        renderOneTree (walletIdx, walletTree) =
            renderTree selection renderWalletTreeItem walletTree
          where
            selection :: Maybe TreePath
            selection = do
                UiWalletTreeSelection{..} <- mSelection
                wtsPath <$ guard (wtsWalletIdx == walletIdx)
        walletImages :: [V.Image]
        walletImages = map renderOneTree $ enumerate wallets
        separator :: V.Image
        separator = V.text V.defAttr ""
        img = V.vertCat $ intersperse separator walletImages
      return $ B.emptyResult
             & B.imageL .~ img

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

data WalletTreeWidgetEvent
  = WalletTreeUpdateEvent [UiWalletTree] (Maybe UiWalletTreeSelection)

handleWalletTreeWidgetEvent
  :: WalletTreeWidgetEvent
  -> StateT WalletTreeWidgetState IO ()
handleWalletTreeWidgetEvent ev = do
  case ev of
    WalletTreeUpdateEvent wallets wselection -> do
      walletTreeWalletsL .= wallets
      walletTreeSelectionL .= wselection

-- | Wallet tree widget and its data model.

module Ariadne.UI.Vty.Widget.WalletTree
       ( WalletTreeWidgetState
       , initWalletTreeWidget
       , drawWalletTreeWidget
       , WalletTreeWidgetEvent(..)
       , handleWalletTreeWidgetEvent
       ) where

import Universum hiding (StateT, (.~), (^.))

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
    -> (SelectionFlag -> TreePath -> V.Attr -> V.Attr -> a -> V.Image)
    -> V.Attr
    -> V.Attr
    -> Tree a
    -> V.Image
renderTree selection toImg defAttr selAttr = go [] []
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
    prefix prefixLines = V.text' defAttr $ mconcat $ map' prefixPart prefixLines
    selectionFlag :: TreePath -> SelectionFlag
    selectionFlag curPath
        | Just curPath == selection = Selected
        | otherwise = NotSelected
    go :: TreePath -> [Bool] -> Tree a -> V.Image
    go curPath prefixLines Node {..} =
        V.vertCat
        ( V.horizJoin (prefix prefixLines)
                      (toImg (selectionFlag curPath) curPath defAttr selAttr rootLabel)
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
    , walletTreeInitialized :: Bool
    }

makeLensesWith postfixLFields ''WalletTreeWidgetState

initWalletTreeWidget :: WalletTreeWidgetState
initWalletTreeWidget = WalletTreeWidgetState [] (Just (UiWalletTreeSelection 0 [0])) False

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

renderWalletTreeItem
  :: SelectionFlag
  -> TreePath
  -> V.Attr
  -> V.Attr
  -> UiWalletTreeItem
  -> V.Image
renderWalletTreeItem selection _ defAttr selAttr UiWalletTreeItem {..} =
  V.text' attr toDisplay
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
            NotSelected -> defAttr
            Selected -> selAttr
    pathText = T.intercalate "-" $ map (pretty . succ) wtiPath

drawWalletTreeWidget
  :: WalletTreeWidgetState
  -> B.Widget name
drawWalletTreeWidget wtws  =
  B.padAll 1 B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    WalletTreeWidgetState wallets mSelection initialized = wtws
    render = do
      c <- B.getContext
      let
        attr = c ^. B.attrL
        selAttr = attr <> B.attrMapLookup "selected" (c ^. B.ctxAttrMapL)
        renderOneTree :: (Word, UiWalletTree) -> V.Image
        renderOneTree (walletIdx, walletTree) =
            renderTree selection renderWalletTreeItem attr selAttr walletTree
          where
            selection :: Maybe TreePath
            selection = do
                UiWalletTreeSelection{..} <- mSelection
                wtsPath <$ guard (wtsWalletIdx == walletIdx)
        walletImages :: [V.Image]
        walletImages = map renderOneTree $ enumerate wallets
        separator :: V.Image
        separator = V.text attr ""
        img
          | null walletImages = V.text attr "No wallets"
          | otherwise = V.vertCat $ intersperse separator walletImages
        imgOrLoading
          | initialized = img
          | otherwise = V.text attr "Loading..."
      return $ B.emptyResult
             & B.imageL .~ imgOrLoading

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
      walletTreeInitializedL .= True
      walletTreeWalletsL .= wallets
      walletTreeSelectionL .= wselection

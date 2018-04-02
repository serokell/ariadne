-- | Wallet tree widget and its data model.

module Ariadne.UI.Vty.Widget.WalletTree
       ( WalletTreeWidgetState
       , initWalletTreeWidget
       , drawWalletTreeWidget
       ) where

import Universum

import Data.List (intersperse)
import Data.Tree (Tree(..))
import Serokell.Util (enumerate)

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face (WalletTree, WalletTreeItem(..))

----------------------------------------------------------------------------
-- General (should probably be moved somewhere at later stage)
----------------------------------------------------------------------------

-- | Path in a 'Tree'.
--
-- N.B. The head of this list is the index in root's children.
-- I find this order more intuitive, but if perfomance turns out
-- to be an issue, we may consider changing it.
type TreePath = [Word]

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
    WalletTreeWidgetState ![WalletTree]

initWalletTreeWidget :: WalletTreeWidgetState
initWalletTreeWidget = WalletTreeWidgetState []

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

renderWalletTreeItem :: SelectionFlag -> TreePath -> WalletTreeItem -> V.Image
renderWalletTreeItem selection _ WalletTreeItem {..} = V.text' attr toDisplay
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
drawWalletTreeWidget _hasFocus (WalletTreeWidgetState wallets) =
  B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    -- Some static data to remove in future.
    wallet1 =
        Node { rootLabel = WalletTreeItem (Just "root1 ADA") [] False
                , subForest = [account11, account12]
                }
    account11 =
        Node { rootLabel = WalletTreeItem Nothing [0] True
             , subForest = map pure [ WalletTreeItem Nothing [0, 0] True
                                    , WalletTreeItem Nothing [0, 1] True]
             }
    account12 =
        Node { rootLabel = WalletTreeItem Nothing [1] True
             , subForest = [pure (WalletTreeItem Nothing [1, 0] True)]
             }
    selectedWallet = Just 0
    selectedNode = Just [1, 0]

    -- Actual rendering.
    render = do
      let
        renderOneTree :: (Word, WalletTree) -> V.Image
        renderOneTree (walletIdx, walletTree) =
            renderTree selection renderWalletTreeItem walletTree
          where
            selection :: Maybe TreePath
            selection = do
                selWallet <- selectedWallet
                selectedNode <* guard (selWallet == walletIdx)
        walletImages :: [V.Image]
        walletImages = map renderOneTree $ enumerate (wallet1:wallets)
        separator :: V.Image
        separator = V.text V.defAttr ""
        img = V.vertCat $ intersperse separator walletImages
      return $ B.emptyResult
             & B.imageL .~ img

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

data WalletTreeCompleted = WalletTreeCompleted | WalletTreeInProgress

data WalletTreeWidgetEvent
  = WalletTreeExit
  | WalletTreeScrollDown
  | WalletTreeScrollUp

handleWalletTreeWidgetEvent
  :: WalletTreeWidgetEvent
  -> StateT WalletTreeWidgetState IO WalletTreeCompleted
handleWalletTreeWidgetEvent ev = do
  case ev of
    WalletTreeExit ->
      return WalletTreeCompleted
    WalletTreeScrollUp ->
      return WalletTreeInProgress
    WalletTreeScrollDown ->
      return WalletTreeInProgress

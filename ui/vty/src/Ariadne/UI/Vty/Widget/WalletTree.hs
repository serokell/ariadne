module Ariadne.UI.Vty.Widget.WalletTree
       ( WalletTreeWidgetState
       , initWalletTreeWidget
       , drawWalletTreeWidget
       ) where

import Universum

import Data.Tree (Tree (..))
import Serokell.Util (enumerate)

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

----------------------------------------------------------------------------
-- General
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
-- Wallet tree
----------------------------------------------------------------------------

data WalletTreeWidgetState = WalletTreeWidgetState

initWalletTreeWidget :: WalletTreeWidgetState
initWalletTreeWidget = WalletTreeWidgetState

type WalletIdx = Word

data WalletTreeItem
    = WTRoot !Text
    -- ^ Root of HD-wallet hierarchy (i. e. a wallet).
    | WTAnother WalletIdx
    -- ^ Another node in HD-wallet hierarchy.

renderWalletTreeItem :: SelectionFlag -> TreePath -> WalletTreeItem -> V.Image
renderWalletTreeItem selection path =
    \case
        WTRoot t -> V.text' attr t
        WTAnother walletIdx -> V.text' attr (pathText walletIdx)
  where
    attr =
        case selection of
            NotSelected -> V.defAttr
            Selected    -> selAttr
    selAttr = V.defAttr `V.withForeColor` V.black `V.withBackColor` V.white
    pathText walletIdx =
        T.intercalate "-" $ map pretty (walletIdx : map succ path)

drawWalletTreeWidget
  :: Bool
  -> WalletTreeWidgetState
  -> B.Widget name
drawWalletTreeWidget _hasFocus WalletTreeWidgetState =
  B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    render = do
      let
        wallet1 =
            Node { rootLabel = WTRoot "root1 ADA"
                 , subForest = [account11, account12]
                 }
        account11 =
            Node { rootLabel = WTAnother 1
                 , subForest = replicate 2 (pure (WTAnother 1))
                 }
        account12 =
            Node { rootLabel = WTAnother 1
                 , subForest = replicate 1 (pure (WTAnother 1))
                 }
        wallet2 =
            Node { rootLabel = WTRoot "root2 ADA"
                 , subForest = [account21]
                 }
        account21 = pure (WTAnother 2)
        img = V.vertCat
            [ renderTree (Just [1, 0]) renderWalletTreeItem wallet1
            , V.text V.defAttr ""
            , renderTree Nothing renderWalletTreeItem wallet2
            ]
      return $ B.emptyResult
             & B.imageL .~ img

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

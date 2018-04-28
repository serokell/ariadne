-- | Wallet tree widget and its data model.

module Ariadne.UI.Vty.Widget.WalletTree
       ( WalletTreeWidgetState
       , initWalletTreeWidget
       , drawWalletTreeWidget
       , WalletTreeWidgetEvent(..)
       , keyToWalletTreeEvent
       , handleWalletTreeWidgetEvent
       ) where

import Universum

import Control.Lens (makeLensesWith, (.=))
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import Data.Tree (Tree(..))
import IiExtras
import Serokell.Util (enumerate)

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard

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
    prefixPart True  False = "│  "
    prefixPart True  True  = "├─ "
    prefixPart False False = "   "
    prefixPart False True  = "└─ "
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
                | wtiShowPath -> ellipsize label <> " (" <> pathText <> ")"
                | otherwise -> ellipsize label
    attr =
        case selection of
            NotSelected -> defAttr
            Selected -> selAttr
    pathText = T.intercalate "-" $ map pretty wtiPath
    ellipsize label
      | length label > 21 = T.take 9 label <> "..." <> T.takeEnd 9 label
      | otherwise = label

drawWalletTreeWidget
  :: Bool
  -> WalletTreeWidgetState
  -> B.Widget name
drawWalletTreeWidget _hasFocus wtws  =
  B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    WalletTreeWidgetState wallets mSelection initialized = wtws
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        selAttr = attr <> B.attrMapLookup "selected" (rdrCtx ^. B.ctxAttrMapL)
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
  | WalletNavigationUp
  | WalletNavigationDown
  | WalletNavigationLeft
  | WalletNavigationRight

keyToWalletTreeEvent
  :: KeyboardEvent
  -> Maybe WalletTreeWidgetEvent
keyToWalletTreeEvent = \case
  KeyUp -> Just WalletNavigationUp
  KeyDown -> Just WalletNavigationDown
  KeyLeft -> Just WalletNavigationLeft
  KeyRight -> Just WalletNavigationRight
  KeyChar 'h' -> Just WalletNavigationLeft
  KeyChar 'j' -> Just WalletNavigationDown
  KeyChar 'k' -> Just WalletNavigationUp
  KeyChar 'l' -> Just WalletNavigationRight
  _ -> Nothing

handleWalletTreeWidgetEvent
  :: UiLangFace
  -> WalletTreeWidgetEvent
  -> StateT WalletTreeWidgetState (B.EventM n) ()
handleWalletTreeWidgetEvent UiLangFace{..} = \case
  WalletTreeUpdateEvent wallets wselection -> do
    walletTreeInitializedL .= True
    walletTreeWalletsL .= wallets
    walletTreeSelectionL .= wselection
  WalletNavigationUp -> defaultSelection $ \selection -> do
      let boundedPred x = if minBound == x then x else pred x
      putSelect (applyToLast boundedPred selection)
  WalletNavigationDown -> defaultSelection $ \selection -> do
      let boundedSucc x = if maxBound == x then x else succ x
      putSelect (applyToLast boundedSucc selection)
  WalletNavigationLeft -> defaultSelection $ \UiWalletTreeSelection{..} ->
      whenJust (nonEmpty wtsPath) $ \path ->
        putSelect (wtsWalletIdx : NE.init path)
  WalletNavigationRight -> defaultSelection $ \UiWalletTreeSelection{..} ->
      putSelect (wtsWalletIdx : (wtsPath ++ [0]))
  where
    putSelect = void . liftIO . langPutCommand . langMkExpr . UiSelect
    applyToLast :: (Word -> Word) -> UiWalletTreeSelection -> [Word]
    applyToLast f UiWalletTreeSelection{..} = case nonEmpty wtsPath of
      Nothing -> [f wtsWalletIdx]
      Just xs -> wtsWalletIdx : (NE.init xs ++ [f (NE.last xs)])
    defaultSelection m = fmap walletTreeSelection get >>= \case
      Nothing -> putSelect [0]
      Just selection -> m selection

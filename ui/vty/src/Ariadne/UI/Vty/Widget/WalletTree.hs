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

import Control.Lens (ix, makeLensesWith, (.=))
import Data.List (findIndex, intercalate)
import Data.Tree (Tree(..))
import IiExtras
import Serokell.Util (enumerate)

import qualified Brick as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.UI

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

-- | State of wallet tree widget, basically the data we want to
-- display (corresponds to a list of wallets).
data WalletTreeWidgetState =
  WalletTreeWidgetState
    { walletTreeItems :: ![TreeItem]
    , walletTreeWallets :: ![UiWalletTree]
    , walletTreeSelection :: !(Maybe UiWalletTreeSelection)
    , walletTreeInitialized :: !Bool
    , walletTreeScrollBySelection :: !Bool
    }

data TreeItemType
  = TreeItemLoading
  | TreeItemSeparator
  | TreeItemPath

data TreeItem =
  TreeItem
    { treeItemType :: !TreeItemType
    , treeItemPrefix :: !Text
    , treeItemLabel :: !Text
    , treeItemPath :: !(Maybe [Word])
    , treeItemSelected :: !Bool
    }

makeLensesWith postfixLFields ''WalletTreeWidgetState

widgetName :: BrickName
widgetName = BrickWalletTree

treeItemLoading, treeItemSeparator :: TreeItem
treeItemLoading = TreeItem TreeItemLoading "" "Loading..." Nothing False
treeItemSeparator = TreeItem TreeItemSeparator "" "" Nothing False

initWalletTreeWidget :: WalletTreeWidgetState
initWalletTreeWidget = WalletTreeWidgetState
  { walletTreeItems = [treeItemLoading]
  , walletTreeWallets = []
  , walletTreeSelection = Just $ UiWalletTreeSelection 0 [0]
  , walletTreeInitialized = False
  , walletTreeScrollBySelection = False
  }

walletsToItems :: [UiWalletTree] -> Maybe UiWalletTreeSelection -> [TreeItem]
walletsToItems wallets selection =
  intercalate [treeItemSeparator] $
  map' (go [] []) (enumerate wallets)
  where
    selPath :: Maybe [Word]
    selPath = do
      UiWalletTreeSelection{..} <- selection
      Just $ wtsWalletIdx : wtsPath

    map' :: (from -> Bool -> to) -> [from] -> [to]
    map' _ [] = []
    map' f [x] = [f x True]
    map' f (x:xs) = f x False : map' f xs

    prefixPart :: Bool -> Bool -> Text
    prefixPart True  False = "│  "
    prefixPart True  True  = "├─ "
    prefixPart False False = "   "
    prefixPart False True  = "└─ "

    ellipsize :: Text -> Text
    ellipsize label
      | length label > 21 = T.take 9 label <> "..." <> T.takeEnd 9 label
      | otherwise = label

    go :: [Word] -> [Bool] -> (Word, UiWalletTree) -> Bool -> [TreeItem]
    go path prefixLines (idx, Node UiWalletTreeItem{..} subForest) isLast =
      TreeItem{..} : concat (map' (go itemPath itemPrefixLines) (enumerate subForest))
      where
        treeItemType = TreeItemPath
        treeItemPrefix = mconcat $ map' prefixPart $ drop 1 itemPrefixLines
        treeItemLabel = pretty idx <> ". " <> maybe "★" ellipsize wtiLabel
        treeItemPath = Just itemPath
        treeItemSelected = selPath == Just itemPath

        itemPath = path ++ [idx]
        itemPrefixLines = prefixLines ++ [not isLast]

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawWalletTreeWidget :: Bool -> WalletTreeWidgetState -> B.Widget BrickName
drawWalletTreeWidget _hasFocus WalletTreeWidgetState{..}  =
  fixedViewport widgetName B.Vertical $
    B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        selAttr = attr <> B.attrMapLookup "selected" (rdrCtx ^. B.ctxAttrMapL)

        img = V.vertCat $ fmap toImg walletTreeItems
        toImg TreeItem{..} = V.horizJoin
          (V.text' attr treeItemPrefix)
          (V.text' (if treeItemSelected then selAttr else attr) treeItemLabel)

        visibilityRequests
          | walletTreeScrollBySelection,
            Just pos <- findIndex treeItemSelected walletTreeItems = [B.VR (B.Location (0, pos)) (1, 1)]
          | otherwise = []
      return $ B.emptyResult
             & B.imageL .~ img
             & B.visibilityRequestsL .~ visibilityRequests

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

data WalletTreeWidgetEvent
  = WalletTreeUpdateEvent [UiWalletTree] (Maybe UiWalletTreeSelection)
  | WalletTreeMouseDownEvent B.Location
  | WalletTreeScrollingEvent ScrollingAction
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
  -> StateT WalletTreeWidgetState (B.EventM BrickName) ()
handleWalletTreeWidgetEvent UiLangFace{..} = \case
  WalletTreeUpdateEvent wallets wselection -> do
    let items = walletsToItems wallets wselection
    unlessM (use walletTreeInitializedL) $ do
      walletTreeInitializedL .= True
      whenJust (items ^? ix 0 >>= treeItemPath) putSelect
    walletTreeItemsL .= items
    walletTreeScrollBySelectionL .= True
  WalletTreeMouseDownEvent (B.Location (_, row)) -> do
    items <- use walletTreeItemsL
    whenJust (items ^? ix row >>= treeItemPath) putSelect
  WalletTreeScrollingEvent action -> do
    lift $ handleScrollingEvent widgetName action
    walletTreeScrollBySelectionL .= False
  WalletNavigationUp -> defaultPath $ applyToLast (\x -> if minBound == x then x else pred x)
  WalletNavigationDown -> defaultPath $ applyToLast (\x -> if maxBound == x then x else succ x)
  WalletNavigationLeft -> defaultPath $ NE.init
  WalletNavigationRight -> defaultPath $ (++ [0]) . toList
  where
    putSelect = void . liftIO . langPutCommand . langMkExpr . UiSelect
    applyToLast :: (Word -> Word) -> NE.NonEmpty Word -> [Word]
    applyToLast f xs = NE.init xs ++ [f (NE.last xs)]
    defaultPath f = get <&> walletTreeItems <&> (find treeItemSelected >=> treeItemPath >=> nonEmpty) <&> maybe [0] f >>= putSelect

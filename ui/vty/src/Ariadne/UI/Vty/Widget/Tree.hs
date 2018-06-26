-- | Wallet tree widget and its data model.

module Ariadne.UI.Vty.Widget.Tree
       ( TreeWidgetState
       , TreeSelection(..)
       , treeWidgetSelection
       , initTreeWidget
       , drawTreeWidget

       , TreeWidgetEvent(..)
       , keyToTreeEvent
       , handleTreeWidgetEvent
       ) where

import Universum

import Control.Lens (ix, makeLensesWith, uses, (.=))
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
data TreeWidgetState =
  TreeWidgetState
    { treeItems :: ![TreeItem]
    , treeSelection :: !(Maybe Int)
    , treeInitialized :: !Bool
    , treeScrollBySelection :: !Bool
    }

data TreeSelection
  = TreeSelectionNone
  | TreeSelectionAddWallet
  | TreeSelectionWallet
  | TreeSelectionAccount
  deriving (Eq)

data TreeItemType
  = TreeItemLoading
  | TreeItemSeparator
  | TreeItemAddWallet
  | TreeItemPath

data TreeItem =
  TreeItem
    { treeItemType :: !TreeItemType
    , treeItemPrefix :: !Text
    , treeItemLabel :: !Text
    , treeItemPath :: !(Maybe [Word])
    , treeItemSelected :: !Bool
    }

makeLensesWith postfixLFields ''TreeWidgetState

widgetName :: BrickName
widgetName = BrickTree

treeItemLoading, treeItemSeparator :: TreeItem
treeItemLoading = TreeItem TreeItemLoading "" "Loading..." Nothing False
treeItemSeparator = TreeItem TreeItemSeparator "" "" Nothing False

treeItemAddWallet :: Bool -> TreeItem
treeItemAddWallet = TreeItem TreeItemAddWallet "" "[ + Add wallet ]" (Just [])

treeWidgetSelection :: TreeWidgetState -> TreeSelection
treeWidgetSelection TreeWidgetState{..} = fromMaybe TreeSelectionNone $ do
  idx <- treeSelection
  item <- treeItems ^? ix idx
  case treeItemType item of
    TreeItemAddWallet -> Just TreeSelectionAddWallet
    TreeItemPath
      | Just [_] <- treeItemPath item -> Just TreeSelectionWallet
      | otherwise -> Just TreeSelectionAccount
    _ -> Nothing

initTreeWidget :: TreeWidgetState
initTreeWidget = TreeWidgetState
  { treeItems = [treeItemLoading]
  , treeSelection = Nothing
  , treeInitialized = False
  , treeScrollBySelection = False
  }

walletsToItems :: [UiTree] -> Maybe UiTreeSelection -> Bool -> [TreeItem]
walletsToItems wallets selection initialized =
  intercalate [treeItemSeparator] $
  [treeItemAddWallet $ initialized && isNothing selection] : map' (go [] []) (enumerate wallets)
  where
    selPath :: Maybe [Word]
    selPath = do
      UiTreeSelection{..} <- selection
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

    go :: [Word] -> [Bool] -> (Word, UiTree) -> Bool -> [TreeItem]
    go path prefixLines (idx, Node UiTreeItem{..} subForest) isLast =
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

drawTreeWidget :: Bool -> TreeWidgetState -> B.Widget BrickName
drawTreeWidget _hasFocus TreeWidgetState{..}  =
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

        img = V.vertCat $ fmap toImg treeItems
        toImg TreeItem{..} = V.horizJoin
          (V.text' attr treeItemPrefix)
          (V.text' (if treeItemSelected then selAttr else attr) treeItemLabel)

        visibilityRequests
          | treeScrollBySelection,
            Just pos <- findIndex treeItemSelected treeItems = [B.VR (B.Location (0, pos)) (1, 1)]
          | otherwise = []
      return $ B.emptyResult
             & B.imageL .~ img
             & B.visibilityRequestsL .~ visibilityRequests

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

data TreeWidgetEvent
  = TreeUpdateEvent [UiTree] (Maybe UiTreeSelection)
  | TreeMouseDownEvent B.Location
  | TreeScrollingEvent ScrollingAction
  | TreeNavigationPrevWallet
  | TreeNavigationNextWallet
  | TreeNavigationParent
  | TreeNavigationFirstChild
  | TreeNavigationPrevItem
  | TreeNavigationNextItem

keyToTreeEvent
  :: KeyboardEvent
  -> Maybe TreeWidgetEvent
keyToTreeEvent = \case
  KeyCtrlUp -> Just TreeNavigationPrevWallet
  KeyCtrlDown -> Just TreeNavigationNextWallet
  KeyUp -> Just TreeNavigationPrevItem
  KeyDown -> Just TreeNavigationNextItem
  KeyLeft -> Just TreeNavigationParent
  KeyRight -> Just TreeNavigationFirstChild
  KeyChar 'h' -> Just TreeNavigationParent
  KeyChar 'j' -> Just TreeNavigationNextItem
  KeyChar 'k' -> Just TreeNavigationPrevItem
  KeyChar 'l' -> Just TreeNavigationFirstChild
  _ -> Nothing

handleTreeWidgetEvent
  :: UiLangFace
  -> TreeWidgetEvent
  -> StateT TreeWidgetState (B.EventM BrickName) ()
handleTreeWidgetEvent UiLangFace{..} = \case
  TreeUpdateEvent wallets wselection -> do
    items <- uses treeInitializedL $ walletsToItems wallets wselection
    unlessM (use treeInitializedL) $ do
      treeInitializedL .= True
      whenJust (items ^? ix 0 >>= treeItemPath) putSelect
    treeItemsL .= items
    treeSelectionL .= findIndex treeItemSelected items
    treeScrollBySelectionL .= True
  TreeMouseDownEvent (B.Location (_, row)) -> do
    items <- use treeItemsL
    whenJust (items ^? ix row >>= treeItemPath) putSelect
  TreeScrollingEvent action -> do
    lift $ handleScrollingEvent widgetName action
    treeScrollBySelectionL .= False

  -- Wallet index is always the head of path
  TreeNavigationPrevWallet -> defaultPath $ \p@(x :| xs) ->
    case xs of
      [] -> if minBound == x then toList p else [pred x]
      _ -> [x]
  TreeNavigationNextWallet -> defaultPath $ \p@(x :| _) -> if maxBound == x then toList p else [succ x]

  TreeNavigationParent -> defaultPath $ NE.init
  TreeNavigationFirstChild -> defaultPath $ (++ [0]) . toList

  TreeNavigationPrevItem -> whenJustM (modifiedPath False) putSelect
  TreeNavigationNextItem -> whenJustM (modifiedPath True) putSelect

  where
    putSelect = void . liftIO . langPutUiCommand . UiSelect
    defaultPath f = get <&> treeItems <&> (find treeItemSelected >=> treeItemPath >=> nonEmpty) <&> maybe [0] f >>= putSelect

    modifiedPath forward = runMaybeT $ do
      selection <- MaybeT $ use treeSelectionL
      items <- use treeItemsL

      let restItems =
            if forward
               then drop (selection + 1) items
               else reverse $ take selection items

      MaybeT $ return $ asum $ map treeItemPath restItems

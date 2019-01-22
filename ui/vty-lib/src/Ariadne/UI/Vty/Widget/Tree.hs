-- | Wallet tree widget and its data model.

module Ariadne.UI.Vty.Widget.Tree
       ( initTreeWidget
       ) where

import Control.Lens (ix, makeLensesWith, uses, (.=))
import Data.List (findIndex)
import Data.Tree (Tree(..))
import Serokell.Util (enumerate)

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.Util

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

-- | State of wallet tree widget, basically the data we want to
-- display (corresponds to a list of wallets).
data TreeWidgetState =
  TreeWidgetState
    { treeLangFace :: !(UiLangFace Vty)
    , treeItems :: ![TreeItem]
    , treeSelection :: !(Maybe Int)
    , treeInitialized :: !Bool
    }

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

treeItemLoading, treeItemSeparator :: TreeItem
treeItemLoading = TreeItem TreeItemLoading "" "Loading..." Nothing False
treeItemSeparator = TreeItem TreeItemSeparator "" "" Nothing False

treeItemAddWallet :: Bool -> TreeItem
treeItemAddWallet = TreeItem TreeItemAddWallet "" "[ + Add wallet ]" (Just [])

initTreeWidget :: UiLangFace Vty -> Widget p
initTreeWidget langFace =
  initWidget $ do
    setWidgetDraw drawTreeWidget
    setWidgetScrollable
    setWidgetHandleKey handleTreeWidgetKey
    setWidgetHandleMouseDown handleTreeWidgetMouseDown
    setWidgetHandleEvent handleTreeWidgetEvent
    setWidgetState TreeWidgetState
      { treeLangFace = langFace
      , treeItems = [treeItemLoading]
      , treeSelection = Nothing
      , treeInitialized = False
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

drawTreeWidget :: TreeWidgetState -> WidgetDrawM TreeWidgetState p WidgetDrawing
drawTreeWidget TreeWidgetState{..} = do
  widgetName <- getWidgetName
  ignoreVisibility <- getIgnoreVisibility
  return . singleDrawing $
    fixedScrollingViewport widgetName B.Vertical $
    B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render ignoreVisibility
      }
  where
    render ignoreVisibility = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        selAttr = attr <> B.attrMapLookup "selected" (rdrCtx ^. B.ctxAttrMapL)

        img = V.pad 1 1 1 1 $ V.vertCat $ fmap toImg treeItems
        toImg TreeItem{..} = V.horizJoin
          (V.text' attr treeItemPrefix)
          (V.text' (if treeItemSelected then selAttr else attr) treeItemLabel)

        visibilityRequests
          | not ignoreVisibility
          , Just pos <- findIndex treeItemSelected treeItems =
              [B.VR (B.Location (0, pos + 1)) (1, 1)]  -- Account for 1-char padding
          | otherwise = []
      return $ B.emptyResult
             & B.imageL .~ img
             & B.visibilityRequestsL .~ visibilityRequests

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleTreeWidgetKey
  :: KeyboardEvent
  -> WidgetEventM TreeWidgetState p WidgetEventResult
handleTreeWidgetKey key = if
    | KeyModUp <- key -> do
        defaultPath $ \p@(x :| xs) ->
          case xs of
            [] -> if minBound == x then toList p else [pred x]
            _ -> [x]
        return WidgetEventHandled
    | KeyModDown <- key -> do
        defaultPath $ \p@(x :| _) -> if maxBound == x then toList p else [succ x]
        return WidgetEventHandled
    | key `elem` [KeyUp, KeyChar 'k'] -> do
        whenJustM (modifiedPath False) performSelect
        return WidgetEventHandled
    | key `elem` [KeyDown, KeyChar 'j'] -> do
        whenJustM (modifiedPath True) performSelect
        return WidgetEventHandled
    | key `elem` [KeyLeft, KeyChar 'h'] -> do
        defaultPath $ \p -> if length p == 1 then toList p else init p
        return WidgetEventHandled
    | key `elem` [KeyRight, KeyChar 'l'] -> do
        defaultPath $ (++ [0]) . toList
        return WidgetEventHandled
    | otherwise ->
        return WidgetEventNotHandled
  where
    defaultPath f =
      getWidgetState <&>
      treeItems <&>
      (find treeItemSelected >=> treeItemPath >=> nonEmpty) <&>
      maybe [0] f >>=
      performSelect

    modifiedPath forward = runMaybeT $ zoomWidgetState $ do
      selection <- MaybeT $ use treeSelectionL
      items <- use treeItemsL

      let restItems =
            if forward
               then drop (selection + 1) items
               else reverse $ take selection items

      MaybeT $ return $ asum $ map treeItemPath restItems

handleTreeWidgetMouseDown
  :: B.Location
  -> WidgetEventM TreeWidgetState p WidgetEventResult
handleTreeWidgetMouseDown (B.Location (_, row)) = do
  items <- use (widgetStateL . treeItemsL)
  whenJust (items ^? ix (row - 1) >>= treeItemPath) performSelect  -- Account for 1-char padding
  return WidgetEventHandled

handleTreeWidgetEvent
  :: UiEvent Vty
  -> WidgetEventM TreeWidgetState p ()
handleTreeWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    items <- uses (widgetStateL . treeInitializedL) $
      walletsToItems wuTrees wuSelection
    unlessM (use (widgetStateL . treeInitializedL)) $ do
      widgetStateL . treeInitializedL .= True
      whenJust (items ^? ix 0 >>= treeItemPath) performSelect
    zoomWidgetState $ do
      treeItemsL .= items
      treeSelectionL .= findIndex treeItemSelected items
  _ ->
    pass

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

performSelect
  :: [Word]
  -> WidgetEventM TreeWidgetState p ()
performSelect path = do
  UiLangFace{..} <- use (widgetStateL . treeLangFaceL)
  void . liftIO . langPutUISilentCommand $ UiSelect path

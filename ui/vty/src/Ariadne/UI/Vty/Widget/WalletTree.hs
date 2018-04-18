-- | Wallet tree widget and its data model.

module Ariadne.UI.Vty.Widget.WalletTree
       ( WalletTreeWidgetState
       , initWalletTreeWidget
       , drawWalletTreeWidget
       , WalletTreeWidgetEvent(..)
       , keyToWalletTreeEvent
       , handleWalletTreeWidgetEvent
       ) where

import Universum hiding (StateT, (.~), get)

import Control.Lens
import Control.Monad.Trans.State
import Data.List (intersperse)
import Data.Tree (Tree(..))
import IiExtras
import Serokell.Util (enumerate)
import qualified Data.List.NonEmpty as NE

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
    , walletTreeInitialized :: Bool
    }

makeLensesWith postfixLFields ''WalletTreeWidgetState

initWalletTreeWidget :: WalletTreeWidgetState
initWalletTreeWidget = WalletTreeWidgetState [] (Just (UiWalletTreeSelection 0 [0])) False

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
    pathText = T.intercalate "-" $ map pretty wtiPath

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
        img
          | null walletImages = V.text V.defAttr "No wallets"
          | otherwise = V.vertCat $ intersperse separator walletImages
        imgOrLoading
          | initialized = img
          | otherwise = V.text V.defAttr "Loading..."
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
          expr = selectExpr (applyToLast boundedPred selection)
      void . liftIO $ langPutCommand expr
  WalletNavigationDown -> defaultSelection $ \selection -> do
      let boundedSucc x = if maxBound == x then x else succ x
          expr = selectExpr (applyToLast boundedSucc selection)
      void . liftIO $ langPutCommand expr
  WalletNavigationLeft -> defaultSelection $ \UiWalletTreeSelection{..} ->
      whenJust (nonEmpty wtsPath) $ \path -> do
        let expr = selectExpr (wtsWalletIdx : NE.init path)
        void . liftIO $ langPutCommand expr
  WalletNavigationRight -> defaultSelection $ \UiWalletTreeSelection{..} -> do
      let expr = selectExpr (wtsWalletIdx : (wtsPath ++ [0]))
      void . liftIO $ langPutCommand expr
  where
    applyToLast :: (Word -> Word) -> UiWalletTreeSelection -> [Word]
    applyToLast f UiWalletTreeSelection{..} = case nonEmpty wtsPath of
      Nothing -> f wtsWalletIdx : []
      Just xs -> wtsWalletIdx : (NE.init xs ++ [f (NE.last xs)])
    -- [AD-70] TODO: Here we are manually constructing the expression string with 'unwords' and then parse it
    -- When some sort of UI AST is implemented this has to be changed.
    selectExpr ws = either (const $ error "impossible") identity (langParse . unwords . ("select":) . map pretty $ ws)
    defaultSelection m = fmap walletTreeSelection get >>= \case
      Nothing -> void . liftIO $ langPutCommand (selectExpr [0])
      Just selection -> m selection

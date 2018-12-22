module Ariadne.UI.Vty.Widget.Form.Edit
       ( EnterMode(..)
       , initBaseEditWidget
       , initEditWidget
       , initPasswordWidget
       , initHiddenPasswordWidget
       ) where

import Control.Lens (makeLensesWith, (.=))
import Data.Char (isSpace)
import Data.List ((!!))
import Data.Text.Zipper (TextZipper)

import qualified Brick as B
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.Util

data EnterMode
  = EnterIgnore
  | EnterNewLine
  | EnterWithBackslash

data EditWidgetState p =
  EditWidgetState
    { editWidgetText :: !Text
    , editWidgetLocation :: !(Int, Int)
    , editWidgetTextZipper :: !(TextZipper Text)
    , editWidgetTextLens :: !(ReifiedLens' p Text)
    , editWidgetLocationLens :: !(Maybe (ReifiedLens' p (Int, Int)))
    , editWidgetAttr :: !B.AttrName
    , editWidgetCharTransform :: !(Maybe (Char -> Char))
    , editWidgetCharAttr :: !(Maybe (p -> (Int, Int) -> B.AttrName))
    , editWidgetEnterMode :: !EnterMode
    }

makeLensesWith postfixLFields ''EditWidgetState

initBaseEditWidget
  :: Lens' p Text
  -> Maybe (ReifiedLens' p (Int, Int))
  -> B.AttrName
  -> Maybe (Char -> Char)
  -> Maybe (p -> (Int, Int) -> B.AttrName)
  -> EnterMode
  -> Widget p
initBaseEditWidget textLens locationLens attr charTransform charAttr enterMode =
  initWidget $ do
    setWidgetDrawWithFocused drawEditWidget
    setWidgetHandleEditKey handleEditWidgetEditKey
    setWidgetHandlePaste handleEditWidgetPaste
    setWidgetHandleMouseDown handleEditWidgetMouseDown
    setWidgetState EditWidgetState
      { editWidgetText = ""
      , editWidgetLocation = (0, 0)
      , editWidgetTextZipper = TZ.textZipper [""] Nothing
      , editWidgetTextLens = Lens textLens
      , editWidgetLocationLens = locationLens
      , editWidgetAttr = attr
      , editWidgetCharTransform = charTransform
      , editWidgetCharAttr= charAttr
      , editWidgetEnterMode = enterMode
      }

initEditWidget :: Lens' p Text -> Widget p
initEditWidget lens = initBaseEditWidget lens Nothing "edit" (Just id) Nothing EnterIgnore

initMultilineEditWidget :: Lens' p Text -> Widget p
initMultilineEditWidget lens = initBaseEditWidget lens Nothing "edit" (Just id) Nothing EnterNewLine

initPasswordWidget :: Lens' p Text -> Widget p
initPasswordWidget lens = initBaseEditWidget lens Nothing "edit" (Just $ const '*') Nothing EnterIgnore

initHiddenPasswordWidget :: Lens' p Text -> Widget p
initHiddenPasswordWidget lens = initBaseEditWidget lens Nothing "edit" Nothing Nothing EnterIgnore

drawEditWidget :: Bool -> EditWidgetState p -> WidgetDrawM (EditWidgetState p) p WidgetDrawing
drawEditWidget _focused widgetState@EditWidgetState{..} = do
  widgetName <- getWidgetName
  parentState <- lift ask
  (ls, (row, col)) <- currentState widgetState
    <$> viewWidgetLens editWidgetTextLens
    <*> mapM viewWidgetLens editWidgetLocationLens
  return . singleDrawing $
    fixedViewport widgetName B.Horizontal $
    B.Widget B.Fixed B.Fixed $ do
      rdrCtx <- B.getContext

      let
        attrMap = rdrCtx ^. B.ctxAttrMapL
        getAttr attrName = B.attrMapLookup attrName attrMap
        defAttr = getAttr editWidgetAttr
        attrFn loc = getAttr $ maybe editWidgetAttr (\fn -> fn parentState loc) editWidgetCharAttr

        chunksOf' w t
          | T.null t = [""]
          | otherwise = T.chunksOf w t

        width = max 1 $ rdrCtx ^. B.availWidthL
        chunks = chunksOf' width <$> ls

        rowToImg r line = case editWidgetCharTransform of
          Nothing -> [ V.text' defAttr $ T.replicate (width) " " ]
          Just charTransform ->
            [ V.horizCat $
              [ V.char (attrFn (r, c)) $ charTransform char
              | (c, char) <- zip [1 + subRow * width..] $ toString subLine
              ] ++
              [ V.text' defAttr $ T.replicate (width - length subLine) " " ]
            | (subRow, subLine) <- zip [0..] line
            ]

        img = V.vertCat $ concat [ rowToImg r line | (r, line) <- zip [1..] chunks ]

        cursor = B.CursorLocation (B.Location location) $ Just widgetName
          where
            row' = min row $ length chunks - 1
            col' = min col $ sum $ map length $ chunks !! row'
            row'' = (sum $ map length $ take row' $ chunks) + col' `div` width
            col'' = col' `rem` width
            location = maybe (0, 0) (const (col'', row'')) editWidgetCharTransform

      return $
        B.emptyResult
          & B.imageL .~ img
          & B.cursorsL .~ [cursor]

handleEditWidgetEditKey
  :: KeyboardEditEvent
  -> WidgetEventM (EditWidgetState p) p WidgetEventResult
handleEditWidgetEditKey = \case
  KeyEditChar c | c /= '\t' -> do
    modifyZipper $ TZ.insertChar c
    return WidgetEventHandled

  KeyEditDelLeft -> do
    modifyZipper $ TZ.deletePrevChar
    return WidgetEventHandled
  KeyEditDelLeftWord -> do
    modifyZipper $ byWord TZ.deletePrevChar TZ.previousChar
    return WidgetEventHandled
  KeyEditDelLeftAll -> do
    modifyZipper $ TZ.killToBOL
    return WidgetEventHandled
  KeyEditDelRight -> do
    modifyZipper $ TZ.deleteChar
    return WidgetEventHandled
  KeyEditDelRightWord -> do
    modifyZipper $ byWord TZ.deleteChar TZ.currentChar
    return WidgetEventHandled

  KeyEditLeft -> do
    modifyZipper $ TZ.moveLeft
    return WidgetEventHandled
  KeyEditLeftWord -> do
    modifyZipper $ byWord TZ.moveLeft TZ.previousChar
    return WidgetEventHandled
  KeyEditRight -> do
    modifyZipper $ TZ.moveRight
    return WidgetEventHandled
  KeyEditRightWord -> do
    modifyZipper $ byWord TZ.moveRight TZ.currentChar
    return WidgetEventHandled
  KeyEditUp -> do
    widgetState@EditWidgetState{..} <- getWidgetState
    (_, (row, _)) <- currentState widgetState
      <$> useWidgetLens editWidgetTextLens
      <*> mapM useWidgetLens editWidgetLocationLens
    if row == 0
      then return WidgetEventNotHandled
      else do
        modifyZipper $ TZ.moveUp
        return WidgetEventHandled
  KeyEditDown -> do
    widgetState@EditWidgetState{..} <- getWidgetState
    (ls, (row, _)) <- currentState widgetState
      <$> useWidgetLens editWidgetTextLens
      <*> mapM useWidgetLens editWidgetLocationLens
    if row == length ls - 1
      then return WidgetEventNotHandled
      else do
        modifyZipper $ TZ.moveDown
        return WidgetEventHandled
  KeyEditHome -> do
    modifyZipper $ TZ.gotoBOL
    return WidgetEventHandled
  KeyEditEnd -> do
    modifyZipper $ TZ.gotoEOL
    return WidgetEventHandled

  KeyEditEnter -> do
    widgetState@EditWidgetState{..} <- getWidgetState
    case editWidgetEnterMode of
      EnterIgnore ->
        return WidgetEventNotHandled
      EnterNewLine -> do
        modifyZipper $ smartBreakLine
        return WidgetEventHandled
      EnterWithBackslash -> do
        zipper <- currentZipper widgetState
          <$> useWidgetLens editWidgetTextLens
          <*> mapM useWidgetLens editWidgetLocationLens
        case TZ.previousChar zipper of
          Just '\\' -> do
            modifyZipper $ smartBreakLine . TZ.deletePrevChar
            return WidgetEventHandled
          _ ->
            return WidgetEventNotHandled

  _ ->
    return WidgetEventNotHandled

handleEditWidgetPaste
  :: Text
  -> WidgetEventM (EditWidgetState p) p WidgetEventResult
handleEditWidgetPaste t = do
  modifyZipper $ TZ.insertMany t
  return WidgetEventHandled

handleEditWidgetMouseDown
  :: B.Location
  -> WidgetEventM (EditWidgetState p) p WidgetEventResult
handleEditWidgetMouseDown (B.Location (col, row)) = do
    EditWidgetState{..} <- getWidgetState
    whenJustM (B.getName <$> get >>= liftBrick . B.lookupViewport) $ \vp -> do
      let width = vp ^. (B.vpSize . _1)
      ls <- T.splitOn "\n" <$> useWidgetLens editWidgetTextLens
      modifyZipper $ safeMoveCursor $ go 0 (row, col - 1) width ls
    return WidgetEventHandled
  where
    rows l width = (T.length l + width - 1) `div` width
    go acc (_, c) _ [] = (acc, c)
    go acc (r, c) width (l:ls)
      | r < rows l width = (acc, width * r + c)
      | otherwise = go (acc + 1) (r - rows l width, c) width ls

currentState :: EditWidgetState p -> Text -> Maybe (Int, Int) -> ([Text], (Int, Int))
currentState EditWidgetState{..} text loc
    | text == editWidgetText && maybe True (== editWidgetLocation) loc = (ls, editWidgetLocation)
    | otherwise = (ls, fromMaybe newLoc loc)
  where
    ls = T.splitOn "\n" text
    row = length ls - 1
    newLoc = (row, T.length $ ls !! row)

currentZipper :: EditWidgetState p -> Text -> Maybe (Int, Int) -> TextZipper Text
currentZipper widgetState@EditWidgetState{..} text loc
    | text == editWidgetText = editWidgetTextZipper
    | otherwise = safeMoveCursor loc' $ TZ.textZipper ls Nothing
  where
    (ls, loc') = currentState widgetState text loc

modifyZipper
  :: (TextZipper Text -> TextZipper Text)
  -> WidgetEventM (EditWidgetState p) p ()
modifyZipper f = do
  widgetState@EditWidgetState{..} <- getWidgetState
  oldText <- useWidgetLens editWidgetTextLens
  oldLocation <- mapM useWidgetLens editWidgetLocationLens
  let
    zipper = f $ currentZipper widgetState oldText oldLocation
    newText = T.intercalate "\n" $ TZ.getText zipper
    newLocation = TZ.cursorPosition zipper
  assignWidgetLens editWidgetTextLens newText
  whenJust editWidgetLocationLens (`assignWidgetLens` newLocation)
  zoomWidgetState $ do
    editWidgetTextL .= newText
    editWidgetLocationL .= newLocation
    editWidgetTextZipperL .= zipper
  when (newText /= oldText) $ widgetEvent WidgetEventEditChanged
  when (maybe False (newLocation /=) oldLocation) $
    widgetEvent WidgetEventEditLocationChanged

safeMoveCursor :: (Int, Int) -> TextZipper Text -> TextZipper Text
safeMoveCursor (row, col) tz = TZ.moveCursor (row', col') tz
  where
    clamp mn mx = max mn . min mx
    lengths = TZ.lineLengths tz
    row' = clamp 0 (length lengths - 1) row
    col' = clamp 0 (lengths !! row') col

byWord
  :: (TextZipper Text -> TextZipper Text)
  -> (TextZipper Text -> Maybe Char)
  -> TextZipper Text
  -> TextZipper Text
byWord move check = go isSpace . go (not . isSpace)
  where
    go p = until (nothingLeft p) move
    nothingLeft p tz = case check tz of
      Nothing -> True
      Just c -> p c

smartBreakLine :: TextZipper Text -> TextZipper Text
smartBreakLine tz =
  let indentation = T.takeWhile isSpace (TZ.currentLine tz)
  in TZ.insertMany indentation (TZ.breakLine tz)

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where
    go x
      | p x = x
      | otherwise = go (f x)

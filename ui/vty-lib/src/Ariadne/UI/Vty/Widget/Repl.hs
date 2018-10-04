module Ariadne.UI.Vty.Widget.Repl
       ( initReplWidget
       ) where

import Universum hiding (head, tail)
import Universum.Unsafe (head, tail)

import Control.Lens (assign, makeLensesWith, traversed, zoom, (.=))
import Named

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Data.Loc as Loc
import qualified Data.Loc.Span as Loc
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Focus
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.Util

type AdaptiveImage =
  Named V.Attr "def_attr" -> Named Int "width" -> V.Image

data OutputElement
  = OutputCommand UiCommandId AdaptiveImage [AdaptiveImage] (Maybe AdaptiveImage)
  | OutputInfo AdaptiveImage

-- Note that fields here are lazy, so we can afford to put all results we might
-- need and discard the existential from 'UiLangFace' without causing excessive
-- recomputation in 'mkReplParseResult'.
data ReplParseResult
  = ReplParseFailure { rpfParseErrDoc :: PP.Doc, rpfParseErrSpans :: [Loc.Span] }
  | ReplParseSuccess { rpfExprDoc :: PP.Doc, rpfPutCommand :: IO UiCommandId }

data ReplWidgetState p =
  ReplWidgetState
    { replWidgetUiFace :: !UiFace
    , replWidgetLangFace :: !UiLangFace
    , replWidgetHistoryFace :: !UiHistoryFace
    , replWidgetCommand :: !Text
    , replWidgetCurrentAutocompletion :: !(Maybe [Text])
    , replWidgetParseResult :: ReplParseResult
    , replWidgetOut :: ![OutputElement]
    , replWidgetFullsizeGetter :: !(p -> Bool)
    }

makeLensesWith postfixLFields ''ReplWidgetState

initReplWidget :: UiFace -> UiLangFace -> UiHistoryFace -> (p -> Bool) -> Widget p
initReplWidget uiFace langFace historyFace fullsizeGetter =
  initWidget $ do
    setWidgetDrawWithFocus drawReplWidget
    setWidgetScrollable
    setWidgetHandleKey handleReplWidgetKey
    setWidgetHandleEvent handleReplWidgetEvent
    setWidgetState ReplWidgetState
      { replWidgetUiFace = uiFace
      , replWidgetLangFace = langFace
      , replWidgetHistoryFace = historyFace
      , replWidgetCommand = ""
      , replWidgetCurrentAutocompletion = Nothing
      , replWidgetParseResult = mkReplParseResult langFace ""
      , replWidgetOut = [OutputInfo ariadneBanner]
      , replWidgetFullsizeGetter = fullsizeGetter
      }

    addWidgetChild WidgetNameReplInput $
      initBaseEditWidget (widgetParentLens replWidgetCommandL) "default" id (Just $ widgetParentGetter spanAttrs) EnterWithBackslash

    addWidgetEventHandler WidgetNameReplInput $ \case
      WidgetEventEditChanged -> do
        reparse
        replWidgetCurrentAutocompletionL .= Nothing
        historyUpdate
      _ -> return ()

    setWidgetFocusList [WidgetNameReplInput]

drawReplWidget :: WidgetName -> ReplWidgetState p -> WidgetDrawM (ReplWidgetState p) p (B.Widget WidgetName)
drawReplWidget focus ReplWidgetState{..} = do
    widget <- ask
    widgetName <- getWidgetName
    fullsize <- replWidgetFullsizeGetter <$> lift ask

    let
      input =
        withFocusIndicator focus widgetName 'R' 0 $
        B.padLeftRight 1 $
        appendPrompt $
        drawWidgetChild focus widget WidgetNameReplInput

    if fullsize
      then return $ B.vBox
        [ B.padLeft (B.Pad 1) $
          scrollingViewport widgetName B.Vertical $
          B.cached widgetName $
          B.Widget
            { B.hSize = B.Fixed
            , B.vSize = B.Fixed
            , B.render = renderFullsize
            }
        , B.hBorder
        , input
        ]
      else return $ B.vBox $ case replWidgetOut of
        (x@OutputCommand{}):_ ->
          [ B.padLeftRight 1 $
            B.Widget
              { B.hSize = B.Fixed
              , B.vSize = B.Fixed
              , B.render = renderSingle x
              }
          , B.hBorder
          , input
          ]
        _ -> [input]
  where
    renderFullsize = do
      rdrCtx <- B.getContext
      let
        defAttr = rdrCtx ^. B.attrL
        width = rdrCtx ^. B.availWidthL
        img =
          V.vertCat $
          intersperse (V.backgroundFill 1 1) $
          drawOutputElement defAttr width <$> reverse replWidgetOut
      return $
        B.emptyResult
          & B.imageL .~ img

    renderSingle el = do
      rdrCtx <- B.getContext
      let
        defAttr = rdrCtx ^. B.attrL
        width = rdrCtx ^. B.availWidthL
        img = drawOutputElement defAttr width el
      return $
        B.emptyResult
          & B.imageL .~ img

    drawOutputElement defAttr width (OutputInfo mkImg) =
      mkImg ! #def_attr defAttr ! #width width
    drawOutputElement defAttr width (OutputCommand commandId commandSrc commandMsgs mCommandOut) =
      let
        cmdInfo = maybe "" (<> " ") (cmdTaskIdRendered commandId)
        prompt = V.text' defAttr "> "
      in
        V.vertCat
          [ V.horizCat
            [ prompt
            , commandSrc
                ! #def_attr defAttr
                ! #width (width - V.imageWidth prompt)
            ]
          , V.vertCat $ reverse commandMsgs <&> \mkImg ->
              mkImg ! #def_attr defAttr ! #width width
          , case mCommandOut of
              Nothing -> V.text' defAttr $ cmdInfo <> "Waiting for result..."
              Just mkImg -> mkImg ! #def_attr defAttr ! #width width
          ]

    inputPrompt = "knit> "
    inputPromptCont = "\n  ... "
    appendPrompt w = B.Widget (B.hSize w) (B.vSize w) $ do
      c <- B.getContext
      result <- B.render $ B.hLimit (c ^. B.availWidthL - T.length inputPrompt) w
      B.render $
        B.hBox
          [ B.txt $ inputPrompt <> T.replicate ((result ^. B.imageL & V.imageHeight) - 1) inputPromptCont
          , B.Widget (B.hSize w) (B.vSize w) (return result)
          ]


handleReplWidgetKey
  :: KeyboardEvent
  -> WidgetEventM (ReplWidgetState p) p WidgetEventResult
handleReplWidgetKey = \case
    KeyQuit -> do
      ReplWidgetState{..} <- get
      if null replWidgetCommand
        then return WidgetEventNotHandled
        else do
          replWidgetCommandL .= ""
          reparse
          return WidgetEventHandled
    KeyAutocomplete -> do
      ReplWidgetState{..} <- get
      options <- case replWidgetCurrentAutocompletion of
        Nothing -> do
          let options = tail $ cycle
                $ replWidgetCommand : langAutocomplete replWidgetLangFace replWidgetCommand
          zoom replWidgetCurrentAutocompletionL $ put $ Just options
          pure options
        Just options -> pure options
      let nextOption = head options
      replWidgetCommandL .= nextOption
      reparse
      replWidgetCurrentAutocompletionL .= Just (tail options)
      return WidgetEventHandled
    KeyEnter -> do
      ReplWidgetState{..} <- get
      if
        | null replWidgetCommand ->
            return ()
        | isQuitCommand replWidgetCommand ->
            liftIO $ putUiEvent replWidgetUiFace $ UiCommandAction UiCommandQuit
        | otherwise -> do
            liftIO $ historyAddCommand replWidgetHistoryFace replWidgetCommand
            case replWidgetParseResult of
              ReplParseFailure{..} -> do
                let out = OutputInfo $ \(Named defAttr) (Named w) -> pprDoc defAttr w rpfParseErrDoc
                zoom replWidgetOutL $ modify (out:)
              ReplParseSuccess{..} -> do
                commandId <- liftIO rpfPutCommand
                let
                  commandSrc (Named defAttr) (Named w) = pprDoc defAttr w rpfExprDoc
                  out = OutputCommand commandId commandSrc [] Nothing
                zoom replWidgetOutL $ modify (out:)
                replWidgetCommandL .= ""
                reparse
            widgetName <- B.getName <$> lift get
            liftBrick $ do
              B.invalidateCacheEntry widgetName
              scrollToEnd widgetName
      return WidgetEventHandled
    KeyUp -> historyNavigate historyPrevCommand
    KeyDown -> historyNavigate historyNextCommand
    _ ->
      return WidgetEventNotHandled
  where
    historyNavigate action = do
      ReplWidgetState{..} <- get
      cmd <- liftIO $ action replWidgetHistoryFace
      whenJust cmd $ assign replWidgetCommandL
      reparse
      return WidgetEventHandled

handleReplWidgetEvent
  :: UiEvent
  -> WidgetEventM (ReplWidgetState p) p ()
handleReplWidgetEvent = \case
  UiCommandEvent commandId commandEvent -> do
    zoom (replWidgetOutL . traversed) $
      modify (updateCommandResult commandId commandEvent)
    widgetName <- B.getName <$> lift get
    liftBrick $ do
      B.invalidateCacheEntry widgetName
      scrollToEnd widgetName
  _ ->
    return ()

reparse :: WidgetEventM (ReplWidgetState p) p ()
reparse = do
  replWidgetCurrentAutocompletionL .= Nothing
  ReplWidgetState{..} <- get
  replWidgetParseResultL .= mkReplParseResult replWidgetLangFace replWidgetCommand

historyUpdate :: WidgetEventM (ReplWidgetState p) p ()
historyUpdate = do
  ReplWidgetState{..} <- get
  liftIO $ historySetPrefix replWidgetHistoryFace replWidgetCommand

mkReplParseResult :: UiLangFace -> Text -> ReplParseResult
mkReplParseResult UiLangFace{..} t =
  case langParse t of
    Left err ->
      ReplParseFailure
        { rpfParseErrDoc = langPpParseError err
        , rpfParseErrSpans = langParseErrSpans err
        }
    Right expr ->
      ReplParseSuccess
        { rpfExprDoc = langPpExpr expr
        , rpfPutCommand = langPutCommand expr
        }

updateCommandResult
  :: UiCommandId
  -> UiCommandEvent
  -> OutputElement
  -> OutputElement
updateCommandResult
  commandId
  commandEvent
  (OutputCommand commandId' commandSrc oldMessages oldResultImage) | eqCommandId commandId commandId'
  = OutputCommand commandId commandSrc messages mCommandResultImage
  where
    eqCommandId = (==) `on` cmdIdEqObject
    mCommandResultImage =
      case commandEvent of
        UiCommandSuccess doc ->
          Just $ \(Named defAttr) (Named w) -> pprDoc defAttr w doc
        UiCommandFailure doc ->
          Just $ \(Named defAttr) (Named w) ->
            V.vertJoin
              (V.text' (defAttr `V.withBackColor` V.red) "Error")
              (pprDoc defAttr w doc)
        UiCommandOutput _ -> oldResultImage
    messages =
      case commandEvent of
        UiCommandSuccess _ -> oldMessages
        UiCommandFailure _ -> oldMessages
        UiCommandOutput doc ->
          let message (Named defAttr) (Named w) = pprDoc defAttr w doc
          in message:oldMessages
updateCommandResult _ _ outCmd = outCmd

spanAttrs :: ReplWidgetState p -> (Int, Int) -> B.AttrName
spanAttrs ReplWidgetState{..} (row, column) = case replWidgetParseResult of
    ReplParseFailure{..} | any inSpan rpfParseErrSpans -> "error"
    _ -> "default"
  where
    inSpan = Loc.overlapping $
      Loc.fromTo
        (Loc.loc (fromIntegral row) (fromIntegral column))
        (Loc.loc (fromIntegral row) (fromIntegral column + 1))

isQuitCommand :: Text -> Bool
isQuitCommand t =
  T.strip t `elem` ["quit", "q", ":quit", ":q", "exit"]

ariadneBanner :: AdaptiveImage
ariadneBanner (Named defAttr) _ = V.vertCat $ map (V.text' defAttr)
  [ "             ___         _           __         "
  , "            /   |  _____(_)___ _____/ /___  ___ "
  , "           / /| | / ___/ / __ `/ __  / __ \\/ _ \\"
  , "          / ___ |/ /  / / /_/ / /_/ / / / /  __/"
  , "         /_/  |_/_/  /_/\\__,_/\\__,_/_/ /_/\\___/ "
  , ""
  , "              Press <Enter> to send a command,"
  , "        <Backslash> <Enter> to insert a line break,"
  , "      <Ctrl+P>/<Ctrl+N> to go to previous/next command,"
  , "       <Tab> to switch between widgets, <Esc> for menu"
  , "     Hint: you can select text with mouse by holding Shift"
  ]

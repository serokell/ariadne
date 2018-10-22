module Knit.Autocompletion
       ( suggestions
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.List (isPrefixOf)
import Data.Loc (loc, spanFromTo)
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Buildable (build)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)
import Text.Earley (fullParses)

import Knit.Argument
import Knit.FormattedExprExt
import Knit.Name
import Knit.Parser
import Knit.ParseTreeExt
import Knit.Prelude
import Knit.Printer
import Knit.Procedure
import Knit.Syntax
import Knit.Tokenizer

data SuggestionCtx = SuggestionCtx
  { _leftSpace :: SpaceWithSelection
  , _rightSpace :: SpaceWithSelection
  }
makeLenses ''SuggestionCtx

instance Default SuggestionCtx where
  def = SuggestionCtx def def

type SuggestionMonad = StateT SuggestionCtx []

-- | Returns completion suggestions assuming that the expression was completed
-- with missing parentheses before parsing.
suggestionExprs
  :: forall components.
     [SomeCommandProc components]
  -> Expr FormattedExprExt CommandId components
  -> SuggestionMonad (Expr FormattedExprExt CommandId components)
suggestionExprs procs = goExpr
  where
    goExpr
      :: Expr FormattedExprExt CommandId components
      -> SuggestionMonad (Expr FormattedExprExt CommandId components)
    goExpr = \case
      lit@(ExprLit _ _) -> pure lit
      ExprProcCall NoExt p -> ExprProcCall NoExt <$> goPc p
      XExpr (ExprInBrackets l e r) -> do
        oldCtx <- get
        leftSpace .= fbSpace l
        rightSpace .= fbSpace r
          <> SpaceWithSelection def (selectionAtTheStart $ fbBracketSelection r)
        e' <- goExpr e
        l' <- uses leftSpace $ \lSpace -> l { fbSpace = lSpace }
        r' <- uses rightSpace $ \rSpace -> FormattedBracket (selectionAtTheEnd rSpace) rSpace
        put oldCtx
        pure $ XExpr $ ExprInBrackets l' e' r'

    goPc
      :: ProcCall' FormattedExprExt CommandId components
      -> SuggestionMonad (ProcCall' FormattedExprExt CommandId components)
    goPc pc@(ProcCall pcSelection cmd args) =
      case cmd of
        CommandIdOperator OpUnit -> do
          lSpace <- use leftSpace
          rSpace <- use rightSpace
          let space = lSpace <> rSpace
          case getSelection $ swsSelection space of
            Nothing -> pure pc
            Just c -> do
              let (lSpace', rSpace') = splitAtCursor c $ getSpace $ swsSpace space
              leftSpace .= SpaceWithSelection (Space lSpace') def
              rightSpace .= SpaceWithSelection (Space rSpace') (Selection $ Just def)
              lift $ pc : map (toProcCall . fst) suggestableProcs
        CommandIdOperator OpAndThen ->
          case args of
            [ArgPos (ArgPosSpace lRightSpace) lhs, ArgPos (ArgPosSpace rLeftSpace) rhs] -> do
              -- lLeftSpace lhs lRightSpace ; rLeftSpace rhs rRightSpace

              rRightSpace <- use rightSpace
              rightSpace .= lRightSpace <> SpaceWithSelection def (selectionAtTheStart pcSelection)
              lhs' <- goExpr lhs
              lRightSpace' <- use rightSpace
              rightSpace .= rRightSpace

              lLeftSpace <- use leftSpace
              leftSpace .= rLeftSpace
              rhs' <- goExpr rhs
              rLeftSpace' <- use leftSpace
              leftSpace .= lLeftSpace

              pure $ ProcCall (selectionAtTheEnd lRightSpace') cmd
                [ ArgPos (ArgPosSpace lRightSpace') lhs'
                , ArgPos (ArgPosSpace rLeftSpace') rhs'
                ]
            _ -> invalidOperatorApplication
        CommandIdName name -> do
          ProcCall pcSelection' cmd' args' <-
            goPcName
              True
              (ProcCall pcSelection name args)
              (ProcCall pcSelection name $ reverse args)
          pure $ ProcCall pcSelection' cmd' $ reverse args'

    goPcName
      :: Bool
      -> ProcCall FormattedExprExt Name (Expr FormattedExprExt CommandId components)
      -> ProcCall FormattedExprExt Name (Expr FormattedExprExt CommandId components)
      -> SuggestionMonad (ProcCall' FormattedExprExt CommandId components)
    goPcName firstCall pc = (>>= goPcRightSpace firstCall pc) . \case

      ProcCall _ _ (XArg xxArg : _) -> absurd xxArg


      ProcCall pcSelection pcName (ArgPos space e : argsRest) -> do
        oldCtx <- get

        rightSpace .= getArgPosSpace space
        (ProcCall pcSelection' pcName' argsRest') <-
          goPcName False pc (ProcCall pcSelection pcName argsRest)
        space' <- use rightSpace

        let
          suggestKeyword
            :: Expr FormattedExprExt CommandId components
            -> SuggestionMonad (Arg' FormattedExprExt CommandId components)
          suggestKeyword (ExprProcCall NoExt (ProcCall aPcSelection (CommandIdName aPcName) []))
            | Just c <- getSelection aPcSelection =
              lift $ ArgPos (ArgPosSpace space') e : do
                let (toComplete, _) = splitAtCursor c $ nameStr aPcName
                pcParam <- procParams pcName
                guard $ isPrefixOf toComplete $ nameStr pcParam
                let selection = Selection $ Just $ cursorAfter $ nameStr pcParam ++ ":"
                pure $ ArgKw (ArgKwSpace space' selection def) pcParam $
                  ExprProcCall NoExt (ProcCall def (CommandIdOperator OpUnit) [])
          suggestKeyword _ = lift []

        put def
        arg' <- suggestKeyword e <|> ArgPos (ArgPosSpace space') <$> goExpr e

        put oldCtx

        pure $ ProcCall pcSelection' pcName' (arg' : argsRest')


      ProcCall pcSelection pcName (ArgKw space keyword e : argsRest) -> do
        oldCtx <- get

        rightSpace .= aksPrefix space
        (ProcCall pcSelection' pcName' argsRest') <-
          goPcName False pc (ProcCall pcSelection pcName argsRest)
        space' <- use rightSpace

        leftSpace .= def
        rightSpace .= def
        e' <- goExpr e

        put oldCtx

        pure $ ProcCall pcSelection' pcName'
          (ArgKw space { aksPrefix = space' } keyword e' : argsRest')


      ProcCall pcSelection pcName [] ->
        case getSelection pcSelection of
          Nothing -> pure $ ProcCall pcSelection (CommandIdName pcName) []
          Just c ->
            let
              pcNameStr = nameStr pcName
              (toComplete, _) = splitAtCursor c pcNameStr
            in
              lift $ (pure $ ProcCall pcSelection (CommandIdName pcName) []) <|> do
                (pcName', _) <- suggestableProcs
                guard $ isPrefixOf toComplete $ nameStr pcName'
                guard $ pcName' /= pcName || c /= cursorAfter pcNameStr
                pure $ toProcCall pcName'

    goPcRightSpace
      :: Bool
      -> ProcCall FormattedExprExt Name (Expr FormattedExprExt CommandId components)
      -> ProcCall' FormattedExprExt CommandId components
      -> SuggestionMonad (ProcCall' FormattedExprExt CommandId components)
    goPcRightSpace firstCall (ProcCall _ pcName _) pc@(ProcCall pcSelection pcCmd pcArgs) = do
      right <- use rightSpace
      case getSelection $ swsSelection right of
        Nothing -> pure pc
        Just c ->
          if c == def || not firstCall && c == cursorAfterSpace (swsSpace right)
            then pure pc
            else pure pc <|> do
              let (lSpace', rSpace') = splitAtCursor c $ getSpace $ swsSpace right
              rightSpace .= SpaceWithSelection (Space rSpace') (Selection $ Just def)
              pcParam <- lift $ procParams pcName
              let
                arg' =
                  ArgKw (ArgKwSpace (SpaceWithSelection (Space lSpace') def) def def) pcParam $
                    ExprProcCall NoExt (ProcCall def (CommandIdOperator OpUnit) [])
              pure $ ProcCall pcSelection pcCmd $ arg' : pcArgs

    selectionAtTheStart :: Selection -> Selection
    selectionAtTheStart selection =
      case getSelection selection of
        Nothing -> def
        Just c ->
          if c == def
            then selection
            else def

    selectionAtTheEnd :: SpaceWithSelection -> Selection
    selectionAtTheEnd sws =
      case getSelection $ swsSelection sws of
        Nothing -> def
        Just c ->
          if cursorAfterSpace (swsSpace sws) == c
            then Selection $ Just def
            else def

    suggestableProcs :: [(Name, [Name])]
    suggestableProcs =
      flip mapMaybe procs $ \(SomeCommandProc CommandProc{..}) ->
        case cpName of
          CommandIdName name -> Just (name, map (^._1) $ getParameters cpArgumentConsumer)
          CommandIdOperator _ -> Nothing

    procParams :: Name -> [Name]
    procParams cmdName = maybe [] snd $ find ((== cmdName) . fst) suggestableProcs

    toProcCall procName = ProcCall
      (Selection $ Just $ cursorAfter $ nameStr procName)
      (CommandIdName procName)
      []

    nameStr = unpack . toLazyText . build

suggestions
  :: forall components proxy.
     ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentTokenToLit components) components
     , AllConstrained (ComponentCommandProcs components) components
     , AllConstrained ComponentPrinter components
     )
  => proxy components
  -> Cursor
  -> T.Text
  -> [(Cursor, T.Text)]
suggestions _ cursor cmd =
  let
    (spaceBefore, tokens) = tokenize cmd

    tokenBalance = \case
      TokenParenthesis bs -> withBracketSide 1 (-1) bs
      _ -> 0
    parensBalance = sum $ map (tokenBalance . _lItem . _twsToken) tokens
    cmdLines = T.splitOn "\n" cmd
    endLoc = (length cmdLines, T.length (last cmdLines) + 1)

    loc' a b = loc (fromIntegral a) (fromIntegral b)
    closingBracket i t = TokenWithSpace
      { _twsToken = Located
        { _lSpan = spanFromTo
            (loc' (fst endLoc) (snd endLoc + i))
            (loc' (fst endLoc) (snd endLoc + i + 1))
        , _lItem = t
        }
      , _twsSpaceAfter = def
      }

    tokens' = tokens ++ zipWith closingBracket [0..]
      (replicate parensBalance (TokenParenthesis BracketSideClosing))
  in
    case fullParses (pExpr @components) tokens' of
      ([], _) -> [(cursor, cmd)]
      (tree:_, _) ->
        let
          (formattedExpr, spaceAfter) = parseTreeToFormattedExpr (Selection $ Just cursor) tree
          makeSws = maybe def $ \space ->
            SpaceWithSelection
              (Space $ toList $ getSkipped $ _lItem space)
              (selectionInSpan (getCursor cursor) (_lSpan space))
          makeRightSws x =
            if cmd == ""
              then SpaceWithSelection def (Selection $ Just def)
              else makeSws x
          processSuggestion (suggestion, SuggestionCtx swsBefore swsAfter) =
            let
              (suggestionStr, selection) = ppFormattedExpr swsBefore suggestion swsAfter
            in
              ( maybe
                  (error "Core invariant violated: no cursor after autocompletion")
                  id
                $ getSelection selection
              , T.dropEnd parensBalance suggestionStr
              )
        in
          map processSuggestion $
            runStateT
              (suggestionExprs commandProcs formattedExpr)
              (SuggestionCtx (makeSws spaceBefore) (makeRightSws spaceAfter))

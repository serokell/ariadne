module Knit.Autocompletion
       ( suggestions
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NonEmpty
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
      ([], _) -> []
      (tree:_, _) ->
        let
          (formattedExpr, spaceAfter) = parseTreeToFormattedExpr (Selection $ Just cursor) tree
          makeSws = maybe def $ \space ->
            SpaceWithSelection
              (Space $ NonEmpty.toList $ getSkipped $ _lItem space)
              (selectionInSpan (getCursor cursor) (_lSpan space))
          processSuggestion ((suggestion, LeftSpace swsBefore), RightSpace swsAfter) =
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
              (runStateT
                (suggestionExprs commandProcs formattedExpr)
                (LeftSpace $ makeSws spaceBefore))
              (RightSpace $ makeSws spaceAfter)

newtype LeftSpace = LeftSpace { getLeftSpace :: SpaceWithSelection }
  deriving (Default)

newtype RightSpace = RightSpace { getRightSpace :: SpaceWithSelection }
  deriving (Default)

type SuggestionMonad = StateT LeftSpace (StateT RightSpace [])

liftLeft :: SuggestionMonad a -> SuggestionMonad a
liftLeft = id

liftRight :: StateT RightSpace [] a -> SuggestionMonad a
liftRight = lift

liftList :: [a] -> SuggestionMonad a
liftList = lift . lift

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
        oldLeft <- liftLeft get
        oldRight <- liftRight get
        liftLeft $ put $ LeftSpace $ fbSpace l
        liftRight $ put $ RightSpace $ fbSpace r
          <> SpaceWithSelection def (selectionAtTheStart $ fbBracketSelection r)
        e' <- goExpr e
        l' <- liftLeft $ gets $ \(LeftSpace lSpace) -> l { fbSpace = lSpace }
        r' <- liftRight $ gets $ \(RightSpace rSpace) ->
          FormattedBracket (selectionAtTheEnd rSpace) rSpace
        liftLeft $ put oldLeft
        liftRight $ put oldRight
        pure $ XExpr $ ExprInBrackets l' e' r'

    goPc
      :: ProcCall' FormattedExprExt CommandId components
      -> SuggestionMonad (ProcCall' FormattedExprExt CommandId components)
    goPc pc@(ProcCall pcSelection cmd args) =
      case cmd of
        CommandIdOperator OpUnit -> do
          leftSpace <- liftLeft $ gets getLeftSpace
          rightSpace <- liftRight $ gets getRightSpace
          let space = leftSpace <> rightSpace
          case getSelection $ swsSelection space of
            Nothing -> pure pc
            Just c -> do
              let (leftSpace', rightSpace') = splitAtCursor c $ getSpace $ swsSpace space
              liftLeft $ put $ LeftSpace $ SpaceWithSelection (Space leftSpace') def
              liftRight $ put $ RightSpace
                $ SpaceWithSelection (Space rightSpace') (Selection $ Just def)
              liftList $ pc : map (toProcCall . fst) suggestableProcs
        CommandIdOperator OpAndThen ->
          case args of
            [ArgPos (ArgPosSpace lRightSpace) lhs, ArgPos (ArgPosSpace rLeftSpace) rhs] -> do
              -- lLeftSpace lhs lRightSpace ; rLeftSpace rhs rRightSpace

              rRightSpace <- liftRight get
              liftRight $ put $ RightSpace $ lRightSpace
                <> SpaceWithSelection def (selectionAtTheStart pcSelection)
              lhs' <- goExpr lhs
              lRightSpace' <- liftRight get
              liftRight $ put rRightSpace

              lLeftSpace <- liftLeft get
              liftLeft $ put $ LeftSpace rLeftSpace
              rhs' <- goExpr rhs
              rLeftSpace' <- liftLeft get
              liftLeft $ put lLeftSpace

              pure $ ProcCall (selectionAtTheEnd $ getRightSpace lRightSpace') cmd
                [ ArgPos (ArgPosSpace $ getRightSpace lRightSpace') lhs'
                , ArgPos (ArgPosSpace $ getLeftSpace rLeftSpace') rhs'
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
        oldRight <- liftRight get
        liftRight $ put $ RightSpace $ getArgPosSpace space
        (ProcCall pcSelection' pcName' argsRest') <-
          goPcName False pc (ProcCall pcSelection pcName argsRest)
        space' <- liftRight $ gets getRightSpace

        let
          suggestKeyword
            :: Expr FormattedExprExt CommandId components
            -> SuggestionMonad (Arg' FormattedExprExt CommandId components)
          suggestKeyword (ExprProcCall NoExt (ProcCall aPcSelection (CommandIdName aPcName) []))
            | Just c <- getSelection aPcSelection =
              liftList $ ArgPos (ArgPosSpace space') e : do
                let (toComplete, _) = splitAtCursor c $ nameStr aPcName
                pcParam <- procParams pcName
                guard $ isPrefixOf toComplete $ nameStr pcParam
                let selection = Selection $ Just $ cursorAfter $ nameStr pcParam ++ ":"
                pure $ ArgKw (ArgKwSpace space' selection def) pcParam $
                  ExprProcCall NoExt (ProcCall def (CommandIdOperator OpUnit) [])
          suggestKeyword _ = liftList []

        oldLeft <- liftLeft get
        liftLeft $ put def
        liftRight $ put def
        arg' <- suggestKeyword e <|> ArgPos (ArgPosSpace space') <$> goExpr e

        liftLeft $ put oldLeft
        liftRight $ put oldRight

        pure $ ProcCall pcSelection' pcName' (arg' : argsRest')


      ProcCall pcSelection pcName (ArgKw space keyword e : argsRest) -> do
        oldRight <- liftRight get
        liftRight $ put $ RightSpace $ aksPrefix space
        (ProcCall pcSelection' pcName' argsRest') <-
          goPcName False pc (ProcCall pcSelection pcName argsRest)
        space' <- liftRight $ gets getRightSpace

        oldLeft <- liftLeft get
        liftLeft $ put def
        liftRight $ put def
        e' <- goExpr e

        liftLeft $ put oldLeft
        liftRight $ put oldRight

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
              liftList $ (pure $ ProcCall pcSelection (CommandIdName pcName) []) <|> do
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
      RightSpace right <- liftRight get
      case getSelection $ swsSelection right of
        Nothing -> pure pc
        Just c ->
          if c == def || not firstCall && c == cursorAfterSpace (swsSpace right)
            then pure pc
            else pure pc <|> do
              let (leftSpace', rightSpace') = splitAtCursor c $ getSpace $ swsSpace right
              liftRight $ put $ RightSpace
                $ SpaceWithSelection (Space rightSpace') (Selection $ Just def)
              pcParam <- liftList $ procParams pcName
              let
                arg' =
                  ArgKw (ArgKwSpace (SpaceWithSelection (Space leftSpace') def) def def) pcParam $
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

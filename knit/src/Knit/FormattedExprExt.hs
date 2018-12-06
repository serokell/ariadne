module Knit.FormattedExprExt
       ( FormattedExprExt

       , Space(..)
       , Cursor(..)
       , Selection(..)
       , SpaceWithSelection(..)
       , FormattedBracket(..)
       , ArgPosSpace(..)
       , ArgKwSpace(..)

       , selectionInSpan
       , cursorAfter
       , cursorAfterSpace
       , zoomInCursor
       , zoomOutCursor
       , splitAtCursor

       , parseTreeToFormattedExpr
       , ppFormattedExpr
       ) where

import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Default
import qualified Data.List.NonEmpty as NonEmpty
import Data.Loc
import Data.Maybe (mapMaybe)
import Data.Semigroup (First(..), Option(..), Semigroup(..), (<>))
import qualified Data.Text as T

import Knit.ParseTreeExt
import Knit.Prelude
import Knit.Printer
import Knit.Syntax
import Knit.Tokenizer

-- | Stores information for printing expressions in their original form.
data FormattedExprExt

type instance XExprProcCall FormattedExprExt _ _ = NoExt
type instance XExprLit FormattedExprExt _ _ = Selection
-- | Space between the brackets and nested expression.
type instance XXExpr FormattedExprExt cmd components =
    ExprInBrackets FormattedBracket (Expr FormattedExprExt cmd components)

-- | Information about the space between procedure and its arguments and between
-- the arguments is stored with the arguments. Their meaning depends on the
-- context. In the case of 'OpAndThen' space between the arguments and semicolon
-- is stored. In the case of 'CommandIdName' each argument stores the space
-- before itself.
type instance XProcCall FormattedExprExt _ _ = (Selection, Completeness)

type instance XArgPos FormattedExprExt _ = ArgPosSpace
type instance XArgKw FormattedExprExt _ = (ArgKwSpace, Completeness)
type instance XXArg FormattedExprExt _ = Void

newtype Space = Space { getSpace :: String }
  deriving (Show, Eq, Ord, Semigroup, Monoid, Default)

newtype Cursor = Cursor { getCursor :: Loc }
  deriving (Show, Eq, Ord)

instance Default Cursor where
  def = Cursor origin

newtype Selection = Selection { getSelection :: Maybe Cursor }
  deriving (Show, Eq, Ord, Default)

data SpaceWithSelection = SpaceWithSelection
  { swsSpace :: Space
  , swsSelection :: Selection
  } deriving (Show, Eq, Ord)

instance Default SpaceWithSelection where
  def = SpaceWithSelection def def

instance Semigroup SpaceWithSelection where
  a <> b = SpaceWithSelection
    { swsSpace = swsSpace a <> swsSpace b
    , swsSelection = coerce @ (Option (First Cursor))
        $  coerce (swsSelection a)
        <> coerce (zoomOutCursor (cursorAfterSpace $ swsSpace a) <$> getSelection (swsSelection b))
    }

instance Monoid SpaceWithSelection where
  mempty = def
  mappend = (<>)

data FormattedBracket = FormattedBracket
  { fbBracketSelection :: Selection
  , fbSpace :: SpaceWithSelection
  } deriving (Show, Eq, Ord)

newtype ArgPosSpace = ArgPosSpace { getArgPosSpace :: SpaceWithSelection }
  deriving (Show, Eq, Ord)

data ArgKwSpace = ArgKwSpace
  { aksPrefix :: SpaceWithSelection
  , aksKwSelection :: Selection
  , aksBetween :: SpaceWithSelection
  } deriving (Show, Eq, Ord)

procedureWithNoName :: a
procedureWithNoName = error "Core invariant violated: procedure with no name"

opUnitWithAName :: a
opUnitWithAName = error "Core invariant violated: OpUnit with a name"

cursorAfter :: String -> Cursor
cursorAfter str
  | (_, _:xs) <- break (=='\n') str =
      let y = getCursor $ cursorAfter xs
      in Cursor $ loc (succ $ locLine y) (locColumn y)
  | otherwise = Cursor $ loc 1 (fromIntegral $ length str + 1)

cursorAfterSpace :: Space -> Cursor
cursorAfterSpace = cursorAfter . getSpace

zoomOutCursor :: Cursor -> Cursor -> Cursor
zoomOutCursor (Cursor oldOrigin) (Cursor c) = Cursor $ loc
    (locLine oldOrigin `add` locLine c)
    (if locLine oldOrigin == 1
      then locColumn oldOrigin `add` locColumn c
      else locColumn c)
  where
    add :: (Num a, Enum a) => a -> a -> a
    add x y = pred $ x + y

zoomInCursor :: Cursor -> Cursor -> Cursor
zoomInCursor (Cursor newOrigin) (Cursor c) = Cursor $ loc
    (locLine c `sub` locLine newOrigin)
    (if locLine c == locLine newOrigin
      then locColumn c `sub` locColumn newOrigin
      else locColumn c)
  where
    sub :: (Num a, Enum a) => a -> a -> a
    sub a b = succ a - b

splitAtCursor :: Cursor -> String -> (String, String)
splitAtCursor (Cursor l) str
  | l == origin = ("", str)
splitAtCursor _ [] = ([], [])
splitAtCursor (Cursor l) str@(x:xs)
  | locLine l == 1 = first (x:) $ splitAtCursor (Cursor $ loc 1 (pred $ locColumn l)) xs
  | otherwise =
    let
      (beforeNewline, afterNewline') = break (== '\n') str
      (newline, afterNewline) = splitAt 1 afterNewline'
    in
      first ((beforeNewline ++ newline) ++)
      $ splitAtCursor (Cursor $ loc (pred $ locLine l) (locColumn l)) afterNewline

selectionInSpan :: Loc -> Span -> Selection
selectionInSpan l s = Selection $
  guard (spanStart s <= l && l <= spanEnd s)
  $> zoomInCursor (Cursor $ spanStart s) (Cursor l)

-- | Converts parse tree to formatted expression alongside with space skipped
-- after parsing provided tree.
parseTreeToFormattedExpr
  :: forall components.
     Selection
  -> Expr ParseTreeExt CommandId components
  -> (Expr FormattedExprExt CommandId components, Maybe (Located Skipped))
parseTreeToFormattedExpr selection' = exprToFormattedExpr
  where
    selection :: Located a -> Selection
    selection = maybe
      (const def)
      (\cursor -> selectionInSpan (getCursor cursor) . _lSpan)
      (getSelection selection')

    skippedWithSelection :: Maybe (Located Skipped) -> SpaceWithSelection
    skippedWithSelection Nothing = SpaceWithSelection def def
    skippedWithSelection (Just t) = SpaceWithSelection
      (Space $ NonEmpty.toList $ getSkipped $ _lItem t)
      (selection t)

    exprToFormattedExpr
      :: Expr ParseTreeExt CommandId components
      -> (Expr FormattedExprExt CommandId components, Maybe (Located Skipped))
    exprToFormattedExpr =
      \case
        XExpr (ExprInBrackets l e r) ->
          let
            (e', se) = exprToFormattedExpr e
          in
            ( XExpr $ ExprInBrackets
                FormattedBracket
                  { fbBracketSelection = selection $ l^.twsToken
                  , fbSpace = skippedWithSelection $ l^.twsSpaceAfter
                  }
                e'
                FormattedBracket
                  { fbBracketSelection = selection $ r^.twsToken
                  , fbSpace = skippedWithSelection se
                  }
            , r^.twsSpaceAfter
            )
        ExprProcCall NoExt pc -> first (ExprProcCall NoExt) (pcToFormattedPc pc)
        ExprLit tok lit -> (ExprLit (selection $ tok^.twsToken) lit, tok^.twsSpaceAfter)

    pcToFormattedPc
      :: ProcCall' ParseTreeExt CommandId components
      -> (ProcCall' FormattedExprExt CommandId components, Maybe (Located Skipped))
    pcToFormattedPc (ProcCall (tok, comp) cmd args) =
      case cmd of
        CommandIdName _ ->
          case tok of
            Just tok' -> first
              (ProcCall (selection $ tok'^.twsToken, comp) cmd)
              (argGo (tok'^.twsSpaceAfter) args)
            Nothing -> procedureWithNoName
        CommandIdOperator op ->
          case (op, tok, args) of
            (OpAndThen, Just tok', [ArgPos NoExt lhs, ArgPos NoExt rhs]) ->
              let
                (lhs', lSpace) = exprToFormattedExpr lhs
                lhs'' = ArgPos (ArgPosSpace $ skippedWithSelection lSpace) lhs'
                (rhs', rSpace) = exprToFormattedExpr rhs
                rhs'' = ArgPos (ArgPosSpace $ skippedWithSelection $ tok'^.twsSpaceAfter) rhs'
              in
                (ProcCall (selection $ tok'^.twsToken, comp) cmd [lhs'', rhs''], rSpace)
            (OpAndThen, Nothing, [_, _]) -> procedureWithNoName

            (OpUnit, Nothing, []) -> (ProcCall (def, comp) cmd [], def)
            (OpUnit, Just _, []) -> opUnitWithAName

            _ -> invalidOperatorApplication

    argToFormattedArg
      :: Maybe (Located Skipped)
      -> Arg' ParseTreeExt CommandId components
      -> (Arg' FormattedExprExt CommandId components, Maybe (Located Skipped))
    argToFormattedArg skippedBefore = \case
      XArg xxArg -> absurd xxArg
      ArgPos NoExt a -> first
        (ArgPos (ArgPosSpace $ skippedWithSelection skippedBefore))
        (exprToFormattedExpr a)
      ArgKw (nameTok, comp) name a ->
        let
          skipped = ArgKwSpace
            { aksPrefix = skippedWithSelection skippedBefore
            , aksKwSelection = selection $ nameTok^.twsToken
            , aksBetween = skippedWithSelection $ nameTok^.twsSpaceAfter
            }
        in
          first (ArgKw (skipped, comp) name) (exprToFormattedExpr a)

    argGo
      :: Maybe (Located Skipped)
      -> [Arg' ParseTreeExt CommandId components]
      -> ([Arg' FormattedExprExt CommandId components], Maybe (Located Skipped))
    argGo skippedBefore [] = ([], skippedBefore)
    argGo skippedBefore (arg : args) =
      let (arg', skippedAfter) = argToFormattedArg skippedBefore arg
      in first (arg' :) (argGo skippedAfter args)

data FormattedChar = FChar Char | FCursor
  deriving (Show, Eq, Ord)

type FormattedString = [FormattedChar]

insertCursor :: Cursor -> String -> FormattedString
insertCursor cursor str =
  let (before, after) = splitAtCursor cursor str
  in map FChar before ++ [FCursor] ++ map FChar after

strWithSelection :: Selection -> String -> FormattedString
strWithSelection = maybe (map FChar) insertCursor . getSelection

convertFormattedOutput :: FormattedString -> (T.Text, Selection)
convertFormattedOutput str =
  ( T.pack $ mapMaybe (\case FChar c -> Just c; FCursor -> Nothing) str
  , findSelection 1 str
  )

findSelection :: Line -> FormattedString -> Selection
findSelection _ [] = def
findSelection line str =
  let (pref, suff) = break (\c -> c == FChar '\n' || c == FCursor) str
  in case suff of
    [] -> def
    (FChar '\n'):rest -> findSelection (succ line) rest
    FCursor:_ -> Selection $ Just $ Cursor $ loc line (fromIntegral $ length pref + 1)
    _ -> error "Core invariant violated: list head does not satisfy predicate after 'break'"

ppFormattedExpr
  :: forall components.
     AllConstrained ComponentPrinter components
  => SpaceWithSelection
  -> Expr FormattedExprExt CommandId components
  -> SpaceWithSelection
  -> (T.Text, Selection)
ppFormattedExpr swsBefore expr swsAfter = convertFormattedOutput $
    ppSpace swsBefore ++ ppFExpr expr ++ ppSpace swsAfter
  where
    ppSpace :: SpaceWithSelection -> FormattedString
    ppSpace (SpaceWithSelection (Space str) c) = strWithSelection c str

    ppFExpr
      :: Expr FormattedExprExt CommandId components
      -> FormattedString
    ppFExpr = \case
      ExprLit c l -> strWithSelection c $ show $ ppLit l
      ExprProcCall NoExt p -> ppProcCall p
      XExpr (ExprInBrackets l e r) ->
           strWithSelection (fbBracketSelection l) "("
        ++ ppSpace (fbSpace l)
        ++ ppFExpr e
        ++ ppSpace (fbSpace r)
        ++ strWithSelection (fbBracketSelection r) ")"

    ppProcCall (ProcCall (c, comp) commandName args) =
      case commandName of
        CommandIdName name -> ppProcedureCall
          (strWithSelection c $ withCompleteness comp $ show $ nameToDoc name)
          args
        CommandIdOperator op -> ppOperatorCall c op args

    ppProcedureCall procName args = procName ++ mconcat (map ppArg args)

    ppArg = \case
      ArgPos (ArgPosSpace prefix) a -> ppSpace prefix ++ ppFExpr a
      ArgKw (ArgKwSpace{..}, comp) name a ->
           ppSpace aksPrefix
        ++ strWithSelection aksKwSelection (withCompleteness comp (show (nameToDoc name)) ++ ":")
        ++ ppSpace aksBetween
        ++ ppFExpr a
      XArg xxArg -> absurd xxArg

    ppOperatorCall _ OpUnit [] = mempty
    ppOperatorCall c OpAndThen [ArgPos (ArgPosSpace ls) l, ArgPos (ArgPosSpace rs) r] =
         ppFExpr l
      ++ ppSpace ls
      ++ strWithSelection c ";"
      ++ ppSpace rs
      ++ ppFExpr r
    ppOperatorCall _ _ _ = invalidOperatorApplication

    withCompleteness :: Completeness -> String -> String
    withCompleteness Complete str = str
    withCompleteness Incomplete str = str ++ "-"

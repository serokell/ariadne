module Knit.Core where

import Data.Text
import Data.Scientific
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Bool
import Data.Char (isAlphaNum)
import Data.List as List
import Control.Applicative as A
import Data.Foldable (asum)
import Data.Vinyl.TypeLevel
import Text.Earley
import Data.Word
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Knit.Value
import Knit.Argument
import Knit.Syntax
import Knit.Procedure
import Knit.Eval
import Knit.Tokenizer
import Knit.Parser
import Knit.Inflate
import Knit.Utils

data Core

data instance ComponentValue components Core
  = ValueBool Bool
  | ValueNumber Scientific
  | ValueString Text
  | ValueUnit
  | ValueFilePath FilePath
  | ValueList [Value components]

deriving instance Eq (Value components) => Eq (ComponentValue components Core)
deriving instance Ord (Value components) => Ord (ComponentValue components Core)
deriving instance Show (Value components) => Show (ComponentValue components Core)

makePrisms 'ValueBool

instance
  ( Elem components Core
  , AllConstrained (ComponentInflate components) components
  ) => ComponentInflate components Core
  where
    componentInflate = \case
      ValueUnit ->
        ExprProcCall $ ProcCall (OperatorName OpUnit) []
      ValueBool b ->
        let commandName = bool "false" "true"   b
        in ExprProcCall $ ProcCall commandName []
      ValueNumber n ->
        ExprLit $ toLit (LitNumber n)
      ValueString s ->
        ExprLit $ toLit (LitString s)
      ValueFilePath s ->
        ExprLit $ toLit (LitFilePath s)
      ValueList vs ->
        ExprProcCall $ ProcCall "L" $
        List.map (ArgPos . inflate) vs

data instance ComponentLit Core
  = LitNumber Scientific
  | LitString Text
  | LitFilePath FilePath
  | LitUnit
  deriving (Eq, Ord, Show)

data instance ComponentToken Core
  = TokenNumber Scientific
  | TokenString String
  | TokenFilePath FilePath
  deriving (Eq, Ord, Show)

makePrisms 'TokenNumber

instance Elem components Core => ComponentTokenizer components Core where
  componentTokenizer =
      [ toToken . TokenNumber <$> pScientific
      , toToken . TokenString <$> pString
      , toToken . TokenFilePath <$> pFilePath
      ]
    where
      pScientific :: Tokenizer Scientific
      pScientific = do
        n <- P.signed (return ()) P.scientific
        p <- isJust <$> P.optional (P.char '%')
        return $ if p then n / 100 else n

      pString :: Tokenizer String
      pString =
        P.char '\"' *>
        P.manyTill (P.charLiteral <|> P.anyChar) (P.char '\"')

      pFilePath :: Tokenizer FilePath
      pFilePath = do
        dots <- P.many (P.char '.')
        cs <-
          (:) <$> P.char '/'
              <*> P.many pFilePathChar
          <|> pure ""
        P.notFollowedBy pFilePathChar
        let path = dots ++ cs
        guard $ not (List.null path)
        return path

      pFilePathChar :: Tokenizer Char
      pFilePathChar =
        P.char '\\' *> P.anyChar <|>
        P.satisfy isFilePathChar

isFilePathChar :: Char -> Bool
isFilePathChar c = isAlphaNum c || c `elem` ['.', '/', '-', '_']

instance Elem components Core => ComponentLitGrammar components Core where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitNumber <$> tok (_Token . uprismElem . _TokenNumber)
      , toLit . LitString . pack <$> tok (_Token . uprismElem . _TokenString)
      , toLit . LitFilePath <$> tok (_Token . uprismElem . _TokenFilePath)
      ]

data instance ComponentCommandRepr components Core
  = CommandIdentity (Value components)

instance ComponentLitToValue components Core where
  componentLitToValue = \case
    LitNumber x -> ValueNumber x
    LitString x -> ValueString x
    LitFilePath x -> ValueFilePath x
    LitUnit -> ValueUnit

data instance ComponentExecContext Core = CoreExecCtx

instance Applicative m => ComponentCommandExec m components Core where
  componentCommandExec CoreExecCtx (CommandIdentity v) = pure v

instance Elem components Core => ComponentCommandProcs components Core where
  componentCommandProcs =
    [ CommandProc
        { cpName = OperatorName OpUnit
        , cpArgumentPrepare = id
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandIdentity (toValue ValueUnit)
        , cpHelp = "The unit operator, does nothing."
        }
    , CommandProc
        { cpName = OperatorName OpSemicolon
        , cpArgumentPrepare = id
        , cpArgumentConsumer =
            getArg tyValue "first" *>
            getArg tyValue "second"
        , cpRepr = CommandIdentity
        , cpHelp = "Execute commands in sequence, taking the result \
                   \of the last one."
        }
    , CommandProc
        { cpName = "true"
        , cpArgumentPrepare = id
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandIdentity (toValue (ValueBool True))
        , cpHelp = "The logical truth value"
        }
    , CommandProc
        { cpName = "false"
        , cpArgumentPrepare = id
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandIdentity (toValue (ValueBool False))
        , cpHelp = "The logical falsehood value."
        }
    , CommandProc
        { cpName = "not"
        , cpArgumentPrepare = id
        , cpArgumentConsumer = getArg tyBool "a"
        , cpRepr = CommandIdentity . toValue . ValueBool . not
        , cpHelp = "The logical negation operator."
        }
    , CommandProc
        { cpName = "L"
        , cpArgumentPrepare = id
        , cpArgumentConsumer = getArgMany tyValue "elem"
        , cpRepr = CommandIdentity . toValue . ValueList
        , cpHelp = "Construct a list."
        }
    ]

infixr `tyEither`

tyEither :: TyProjection components a -> TyProjection components b -> TyProjection components (Either a b)
tyEither tpa tpb = TyProjection
    { tpTypeName = TypeNameEither (tpTypeName tpa) (tpTypeName tpb)
    , tpMatcher = \v ->
        Left <$> tpMatcher tpa v <|>
        Right <$> tpMatcher tpb v
    }

tyValue :: TyProjection components (Value components)
tyValue = TyProjection "Value" Just

tyBool :: Elem components Core => TyProjection components Bool
tyBool = TyProjection "Bool" (preview _ValueBool <=< fromValue)

tyFilePath :: Elem components Core => TyProjection components FilePath
tyFilePath = TyProjection "FilePath" (preview _ValueFilePath <=< fromValue)

tyInt :: Elem components Core => TyProjection components Int
tyInt = TyProjection "Int" (toBoundedInteger <=< preview _ValueNumber <=< fromValue)

tyWord :: Elem components Core => TyProjection components Word
tyWord = TyProjection "Word" (toBoundedInteger <=< preview _ValueNumber <=< fromValue)

tyWord8 :: Elem components Core => TyProjection components Word8
tyWord8 = TyProjection "Word8" (toBoundedInteger <=< preview _ValueNumber <=< fromValue)

tyWord32 :: Elem components Core => TyProjection components Word32
tyWord32 = TyProjection "Word32" (toBoundedInteger <=< preview _ValueNumber <=< fromValue)

tyWord64 :: Elem components Core => TyProjection components Word64
tyWord64 = TyProjection "Word64" (toBoundedInteger <=< preview _ValueNumber <=< fromValue)

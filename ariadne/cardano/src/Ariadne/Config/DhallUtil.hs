module Ariadne.Config.DhallUtil
       ( defalultIfNothing
       , parseField
       , interpretFilePath
       , interpretWord16
       , interpretInt
       , interpretBytestringUTF8
       , interpretByte
       , interpretTime
       , injectInt
       , injectFilePath
       , injectByteStringUTF8
       , injectWord16
       , injectByte
       , injectMaybe
       , injectTime
       , toDhall
       , fromDhall
       ) where

import Data.Functor.Contravariant (Contravariant(..))
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.String (fromString)
import qualified Dhall as D
import Dhall.Core (Expr(..))
import qualified Dhall.Core as Core
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import qualified Numeric.Natural as Numeric
import Serokell.Data.Memory.Units (Byte, fromBytes)
import Time (Rat, Time(..))

defalultIfNothing :: (Monad m) => a -> m (Maybe a) -> m a
defalultIfNothing def_ mMba = fromMaybe def_ <$> mMba

type FieldNameModifier = D.Text -> D.Text

parseField ::
       FieldNameModifier
    -> Map.InsOrdHashMap D.Text (Expr Src X)
    -> D.Text
    -> D.Type a
    -> Maybe a
parseField fm dhallRec name type_ = Map.lookup (fm name) dhallRec >>= D.extract type_

interpretFilePath :: D.Type FilePath
interpretFilePath = toString <$> D.lazyText

interpretWord16 :: D.Type Word16
interpretWord16 = (fromIntegral . toInteger) <$> D.natural

interpretTime :: forall (unit :: Rat) . D.Type (Time unit)
interpretTime = (Time . realToFrac) <$> D.double

interpretInt :: D.Type Int
interpretInt = fromIntegral <$> D.integer -- overflow problem?

-- It makes sence to have different ByteString Inject/Interpret
-- impementetions for different structures.
interpretBytestringUTF8 :: D.Type ByteString
interpretBytestringUTF8 = encodeUtf8 <$> D.strictText

interpretByte :: D.Type Byte
interpretByte = (fromBytes . toInteger) <$> D.natural

injectInt :: D.InputType Int
injectInt = D.InputType {..}
  where
    embed = IntegerLit . toInteger
    declared = Integer

injectFilePath :: D.InputType FilePath
injectFilePath = D.InputType {..}
  where
    embed fp =
        TextLit $ fromString fp
    declared = Text

injectByteStringUTF8 :: D.InputType ByteString
injectByteStringUTF8 = contramap decodeUtf8 (D.inject :: D.InputType D.Text)

injectWord16 :: D.InputType Word16
injectWord16 = contramap (fromIntegral . toInteger) (D.inject :: D.InputType Numeric.Natural)

injectByte :: D.InputType Byte
injectByte = contramap (fromIntegral . toInteger) (D.inject :: D.InputType Numeric.Natural)

injectTime :: forall (unit :: Rat) . D.InputType (Time unit)
injectTime = contramap (realToFrac . unTime) (D.inject :: D.InputType Double)

injectMaybe :: D.InputType a -> D.InputType (Maybe a)
injectMaybe innerInpType = D.InputType embedOut declaredOut
      where
        embedOut (Just x) =
            OptionalLit declaredIn (pure (embedIn x))
        embedOut Nothing =
            OptionalLit declaredIn  empty

        D.InputType embedIn declaredIn = innerInpType

        declaredOut = App Optional declaredIn

toDhall :: (D.Inject a) => a -> D.Text
toDhall x = Core.pretty $ D.embed D.inject x

fromDhall :: (D.Interpret a) => D.Text -> IO a
fromDhall inp = D.detailed $ D.input D.auto inp

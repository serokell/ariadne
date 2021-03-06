module Knit.Name
       ( Letter(getLetter)
       , unsafeMkLetter
       , Name(..)
       , unsafeMkName
       ) where

import Prelude

import Data.Char (isAlpha)
import Data.Coerce (coerce)
import Data.List.NonEmpty as NonEmpty
import Data.List.Split (splitWhen)
import Data.String (IsString(..))
import Data.Text.Buildable as Buildable
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary.Generic
  (Arbitrary(..), genericArbitrary, genericShrink)
import Test.QuickCheck.Gen (suchThat)
import Test.QuickCheck.Instances ()

-- | Invariant: @isAlpha . getLetter = const True@
newtype Letter = Letter { getLetter :: Char }
    deriving (Eq, Ord, Show)

unsafeMkLetter :: Char -> Letter
unsafeMkLetter = Letter

instance Arbitrary Letter where
    arbitrary = Letter <$> arbitrary `suchThat` isAlpha

newtype Name = Name (NonEmpty (NonEmpty Letter))
    deriving (Eq, Ord, Generic)

unsafeMkName :: [String] -> Name
unsafeMkName = coerce . fmap NonEmpty.fromList . NonEmpty.fromList

instance Arbitrary Name where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Buildable Name where
    build
        = foldMap (fromString . toList)
        . NonEmpty.intersperse ('-' :| [])
        . fromLetterNENE
      where
        fromLetterNENE :: Name -> NonEmpty (NonEmpty Char)
        fromLetterNENE = coerce

instance Show Name where
    showsPrec n = showsPrec n . build

-- | Unsafe, requires manual validation.
instance IsString Name where
    fromString = unsafeMkName . splitWhen (=='-')

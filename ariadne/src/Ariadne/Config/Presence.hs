module Ariadne.Config.Presence
  ( Presence (There, File)
  , _File
  ) where

import Universum

import Control.Lens (Prism', prism')

data Presence a
  = There a
  | File (Maybe FilePath)
  deriving (Eq, Show)

instance Functor Presence where
  fmap f (There x) = There $ f x
  fmap _ (File mfp) = File mfp

instance Applicative Presence where
  pure x = There x
  There f <*> x = f <$> x
  File x <*> _ = File x

instance Alternative Presence where
  empty = File Nothing
  There f <|> _ = There f
  File _ <|> x = x

_File :: Prism' (Presence a) FilePath
_File = prism' (File . Just) to
  where
    to :: Presence a -> Maybe FilePath
    to (File mfp) = mfp
    to (There _) = Nothing

{-# LANGUAGE AllowAmbiguousTypes #-}

module IiExtras
  (
  -- * Definitions
    type (~>)
  , (:~>)(..)
  , postfixLFields
  , integralDistribExcess
  , atomicRunStateIORef'
  , umapConstrained
  , Elem
  , elemEv
  , rgetElem
  , uliftElem
  , umatchElem
  , uprismElem
  , ufold
  , Some(..)
  , Spine
  , KnownSpine(..)
  , relemsproxy

  -- * Parsing
  , longestMatch

  -- * Re-exports
  , Union(..)
  , Rec(..)
  , RecAll
  , Proxy(..)
  , AllConstrained
  ) where

import Control.Applicative (optional)
import Control.Applicative as A
import Control.Lens
import Control.Monad.Trans.State
import Data.Function (on)
import Data.IORef
import Data.List (maximumBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import Data.Proxy
import Data.Traversable (for)
import Data.Tuple
import Data.Type.Equality
import Data.Union
import Data.Vinyl.Core hiding (Dict)
import Data.Vinyl.TypeLevel
import Prelude
import Text.Megaparsec
  (MonadParsec, getParserState, getPosition, lookAhead, try, updateParserState)

type f ~> g = forall x. f x -> g x

newtype f :~> g = Nat { runNat :: f ~> g }

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])

integralDistribExcess :: Integral n => n -> n -> (n, n)
integralDistribExcess desired actual = (l, r)
  where
    excess =
      if desired > actual
      then desired - actual
      else 0
    l = excess `quot` 2
    r = excess - l

-- | Atomically modifies the contents of an 'IORef' using the provided 'State'
-- action. Forces both the value stored in the 'IORef' as well as the value
-- returned.
atomicRunStateIORef' :: IORef s -> State s a -> IO a
atomicRunStateIORef' ref st = atomicModifyIORef' ref (swap . runState st)

umapConstrained
  :: forall c f g as.
     AllConstrained c as
  => (forall a . c a => f a -> g a)
  -> Union f as
  -> Union g as
umapConstrained f = \case
  This a -> This (f a)
  That u -> That (umapConstrained @c f u)

class RIndex a xs ~ n => Elem' n a xs where
  elemEv' :: Union ((:~:) a) xs

instance (xs ~ (a : xs')) => Elem' 'Z a xs where
  elemEv' = This Refl

instance
    ( RIndex a (x : xs') ~ 'S n
    , xs ~ (x : xs')
    , Elem' n a xs'
    ) => Elem' ('S n) a xs
  where
    elemEv' = That elemEv'

class Elem' (RIndex x xs) x xs => Elem xs x
instance Elem' (RIndex x xs) x xs => Elem xs x

elemEv :: forall xs a. Elem xs a => Union ((:~:) a) xs
elemEv = elemEv'

rgetElem :: forall f xs a. Elem xs a => Rec f xs -> f a
rgetElem = go (elemEv @xs @a)
  where
    go
      :: forall xs'.
         Union ((:~:) a) xs'
      -> Rec f xs'
      -> f a
    go (This Refl) (fa :& _) = fa
    go (That i) (_ :& fxs) = go i fxs

uliftElem
  :: forall f xs a.
     Elem xs a
  => f a
  -> Union f xs
uliftElem v = umap (\Refl -> v) (elemEv @xs @a)

umatchElem
  :: forall f xs a.
     Elem xs a
  => Union f xs
  -> Maybe (f a)
umatchElem = go (elemEv @xs @a)
  where
    go
      :: forall xs'.
         Union ((:~:) a) xs'
      -> Union f xs'
      -> Maybe (f a)
    go (This Refl) (This v) = Just v
    go (That i) (That v) = go i v
    go _ _ = Nothing

uprismElem :: forall f xs a. Elem xs a => Prism' (Union f xs) (f a)
uprismElem = prism' uliftElem umatchElem

ufold
  :: forall c f xs r.
      AllConstrained c xs
  => (forall x. c x => f x -> r)
  -> Union f xs
  -> r
ufold f (This v) = f v
ufold f (That v) = ufold @c f v

data Some c f where
  Some :: c x => f x -> Some c f

type Spine = Rec Proxy

class KnownSpine xs where
  knownSpine :: Spine xs

instance KnownSpine '[] where
  knownSpine = RNil

instance KnownSpine xs => KnownSpine (x:xs) where
  knownSpine = Proxy :& knownSpine

relemsproxy :: Rec f xs -> Proxy xs
relemsproxy = const Proxy

longestMatch :: MonadParsec e s m => [m a] -> m a
longestMatch ps = do
  ps' <-
    for ps $ \p ->
        optional . try . lookAhead $ do
            datum <- p
            position <- getPosition
            pState <- getParserState
            return (position, (pState, datum))
  case nonEmpty (catMaybes ps') of
    Nothing -> A.empty
    Just ps'' -> do
        let tup = snd $ maximumBy (compare `on` fst) ps''
        applyParser tup
      where
        applyParser (pState, datum) = do
            updateParserState (const pState)
            return datum

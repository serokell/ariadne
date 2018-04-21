{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Argument
       ( ArgumentError(..)
       , isEmptyArgumentError
       , TypeName(..)
       , TyProjection(..)
       , TypeError(..)
       , ProcError(..)
       , isEmptyProcError
       , ArgumentConsumer
       , consumeArguments
       , ArgCardinality(..)
       , SomeArgCardinality(..)
       , getArg
       , getArgOpt
       , getArgMany
       , getArgSome
       , getParameters
       , typeDirectedKwAnn
       ) where

import Prelude
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List (sortOn)
import Data.List.NonEmpty
import Data.Maybe
import Data.Monoid
import Data.Set as Set
import Data.String
import Data.Text
import Numeric.Natural (Natural)

import Knit.Name (Name(..))
import Knit.Syntax (Arg(..))
import Knit.Value (Value)

data ArgumentError = ArgumentError
    { aeMissingKeys    :: !(Set Name)
    , aeIrrelevantKeys :: !(Set Name)
    , aeIrrelevantPos  :: !Natural
    } deriving (Eq, Ord, Show)

isEmptyArgumentError :: ArgumentError -> Bool
isEmptyArgumentError ArgumentError{..} =
    Set.null aeMissingKeys && Set.null aeIrrelevantKeys && aeIrrelevantPos == 0

instance Monoid ArgumentError where
    mempty = ArgumentError Set.empty Set.empty 0
    mappend (ArgumentError m1 i1 p1) (ArgumentError m2 i2 p2) =
        ArgumentError (Set.union m1 m2) (Set.union i1 i2) (p1 + p2)

data TypeName = TypeName Text | TypeNameEither TypeName TypeName
    deriving (Eq, Ord, Show)

instance IsString TypeName where
    fromString = TypeName . fromString

data TypeError components = TypeError
    { teExpectedType :: !TypeName
    , teActualValue  :: !(Value components)
    }

deriving instance Eq (Value components) => Eq (TypeError components)
deriving instance Ord (Value components) => Ord (TypeError components)
deriving instance Show (Value components) => Show (TypeError components)

data ProcError components = ProcError
    { peArgumentError :: !ArgumentError
    , peTypeErrors    :: !(Set (TypeError components))
    }

deriving instance Eq (Value components) => Eq (ProcError components)
deriving instance Ord (Value components) => Ord (ProcError components)
deriving instance Show (Value components) => Show (ProcError components)

isEmptyProcError :: ProcError components -> Bool
isEmptyProcError ProcError{..} =
    isEmptyArgumentError peArgumentError && Set.null peTypeErrors

instance Ord (Value components) => Monoid (ProcError components) where
    mempty = ProcError mempty Set.empty
    mappend (ProcError a1 t1) (ProcError a2 t2) =
        ProcError (mappend a1 a2) (Set.union t1 t2)

data ArgumentConsumerState components = ACS
    { acsRemaining :: ![Arg (Value components)]
    , acsError     :: !(ProcError components)
    }

deriving instance Eq (Value components) => Eq (ArgumentConsumerState components)
deriving instance Ord (Value components) => Ord (ArgumentConsumerState components)
deriving instance Show (Value components) => Show (ArgumentConsumerState components)

data ArgCardinality f where
  ArgCardSingle :: ArgCardinality Identity
  ArgCardOpt :: ArgCardinality Maybe
  ArgCardMany :: ArgCardinality []
  ArgCardSome :: ArgCardinality NonEmpty

data SomeArgCardinality = forall f. SomeArgCardinality (ArgCardinality f)

argCardC :: forall c f r.
     (c Identity, c Maybe, c [], c NonEmpty)
  => ArgCardinality f
  -> (c f => r)
  -> r
argCardC argCard r = case argCard of
    ArgCardSingle -> r
    ArgCardOpt    -> r
    ArgCardMany   -> r
    ArgCardSome   -> r

data TyProjection components a = TyProjection
    { tpTypeName :: !TypeName
    , tpMatcher  :: !(Value components -> Maybe a)
    } deriving (Functor)

data ArgumentConsumer components a where
    GetArg
      :: ArgCardinality f
      -> (TyProjection components a)
      -> Name
      -> ArgumentConsumer components (f a)
    AcPure :: a -> ArgumentConsumer components a
    AcAp
      :: ArgumentConsumer components (a -> b)
      -> ArgumentConsumer components a
      -> ArgumentConsumer components b

instance Functor (ArgumentConsumer components) where
    fmap f x = pure f <*> x

instance Applicative (ArgumentConsumer components) where
    pure = AcPure
    (<*>) = AcAp

getArg
  :: (TyProjection components a)
  -> Name
  -> ArgumentConsumer components a
getArg tyProj name = fmap runIdentity (getArgSingle tyProj name)

getArgSingle
  :: (TyProjection components a)
  -> Name
  -> ArgumentConsumer components (Identity a)
getArgSingle = GetArg ArgCardSingle

getArgOpt
  :: (TyProjection components a)
  -> Name
  -> ArgumentConsumer components (Maybe a)
getArgOpt = GetArg ArgCardOpt

getArgMany
  :: (TyProjection components a)
  -> Name
  -> ArgumentConsumer components [a]
getArgMany = GetArg ArgCardMany

getArgSome
  :: (TyProjection components a)
  -> Name
  -> ArgumentConsumer components (NonEmpty a)
getArgSome = GetArg ArgCardSome

runArgumentConsumer :: forall components a.
       Ord (Value components)
    => ArgumentConsumer components a
    -> ArgumentConsumerState components
    -> (Maybe a, ArgumentConsumerState components)
runArgumentConsumer ac acs = case ac of
    GetArg argCard tp key -> argCardC @Traversable argCard $
      case lookupArgWithCard argCard key (acsRemaining acs) of
        Left argError ->
          let
            procError = mempty { peArgumentError = argError }
            acs' = acs { acsError = acsError acs `mappend` procError }
          in
            (Nothing, acs')
        Right (fv, remaining') ->
            let
              acs' = acs { acsRemaining = remaining' }
              (mResults, acs'') = flip runState acs' $
                forM fv $ \v -> do
                  let
                    mResult = tpMatcher @components tp v
                    typeError = TypeError
                      { teExpectedType = tpTypeName tp
                      , teActualValue = v }
                    procError = (mempty @(ProcError components))
                      { peTypeErrors = Set.singleton typeError }
                  unless (isJust mResult) $ modify $ \a ->
                    a { acsError = acsError a `mappend` procError }
                  return mResult
            in
              (sequenceA mResults, acs'')
    AcPure a -> (Just a, acs)
    AcAp ac1 ac2 ->
        let
            (mResult1, acs') = runArgumentConsumer ac1 acs
            (mResult2, acs'') = runArgumentConsumer ac2 acs'
            mResult = mResult1 <*> mResult2
        in
            (mResult, acs'')

lookupArgWithCard ::
       ArgCardinality f
    -> Name
    -> [Arg (Value components)]
    -> Either ArgumentError (f (Value components), [Arg (Value components)])
lookupArgWithCard argCard name args = case argCard of
    ArgCardSingle -> over _1 Identity <$> lookupArgSingle name args
    ArgCardOpt    -> Right $ lookupArgOpt name args
    ArgCardMany   -> Right $ lookupArgMany name args
    ArgCardSome   -> lookupArgSome name args

lookupArgSingle ::
       Name
    -> [Arg (Value components)]
    -> Either ArgumentError ((Value components), [Arg (Value components)])
lookupArgSingle name args = do
    let (mValue, args') = lookupArgOpt name args
    case mValue of
        Nothing -> Left mempty { aeMissingKeys = Set.singleton name }
        Just v  -> return (v, args')

lookupArgOpt ::
       Name
    -> [Arg (Value components)]
    -> (Maybe (Value components), [Arg (Value components)])
lookupArgOpt name = \case
    [] -> (Nothing, [])
    ArgPos a : args -> (Just a, args)
    arg@(ArgKw name' a) : args ->
        if name == name'
        then (Just a, args)
        else over _2 (arg:) $ lookupArgOpt name args

lookupArgMany ::
       Name
    -> [Arg (Value components)]
    -> ([(Value components)], [Arg (Value components)])
lookupArgMany name = \case
    [] -> ([], [])
    ArgPos a : args -> over _1 (a:) $ lookupArgMany name args
    arg@(ArgKw name' a) : args ->
        if name == name'
        then over _1 (a:) $ lookupArgMany name args
        else over _2 (arg:) $ lookupArgMany name args

lookupArgSome ::
       Name
    -> [Arg (Value components)]
    -> Either ArgumentError (NonEmpty (Value components), [Arg (Value components)])
lookupArgSome name args = do
    (v, args') <- lookupArgSingle name args
    return $ over _1 (v :|) $ lookupArgMany name args'

consumeArguments ::
       Ord (Value components)
    => ArgumentConsumer components a
    -> [Arg (Value components)]
    -> Either (ProcError components) a
consumeArguments ac args =
    let
        sortedArgs = sortOn isArgPos args -- stable sort, kw arguments first
        acs = ACS { acsRemaining = sortedArgs, acsError = mempty }
        (mResult, acs') = runArgumentConsumer ac acs
        procError = acsError acs' `mappend` irrelevanceError
        irrelevanceError = mempty { peArgumentError = toIrrelevanceError (acsRemaining acs') }
        mResult' = mResult <* guard (isEmptyProcError procError)
    in
        case mResult' of
            Nothing -> Left procError
            Just a  -> Right a

isArgPos :: Arg a -> Bool
isArgPos = \case
    ArgPos _ -> True
    _ -> False

toIrrelevanceError
  :: [Arg (Value components)]
  -> ArgumentError
toIrrelevanceError = foldMap $ \case
    ArgPos _ -> mempty { aeIrrelevantPos = 1 }
    ArgKw key _ -> mempty { aeIrrelevantKeys = Set.singleton key }

getParameters
  :: ArgumentConsumer components a
  -> [(Name, TypeName, SomeArgCardinality)]
getParameters = \case
    GetArg ac tp name -> [(name, tpTypeName tp, SomeArgCardinality ac)]
    AcPure _ -> []
    AcAp f x -> getParameters f <> getParameters x

typeDirectedKwAnn
  :: Name
  -> (TyProjection components a)
  -> Arg (Value components)
  -> Arg (Value components)
typeDirectedKwAnn name tp arg = case arg of
    ArgPos v | isJust (tpMatcher tp v) -> ArgKw name v
    _ -> arg

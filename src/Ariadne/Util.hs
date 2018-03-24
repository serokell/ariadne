module Ariadne.Util where

import Prelude
import Control.Lens
import Data.Typeable
import Control.Monad.Trans.State
import qualified Brick.Focus as B

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])

currentFocus :: forall a. Typeable a => B.FocusRing a -> a
currentFocus focusRing =
  case B.focusGetCurrent focusRing of
    Nothing -> error $
      "currentFocus @" ++ show (typeRep (Proxy @a)) ++
      ": impossible, no focused layer"
    Just a -> a

wrapBrickHandler :: Functor m => (e -> s -> m s) -> e -> StateT s m ()
wrapBrickHandler f ev = StateT $ \s -> ((),) <$> f ev s

integralDistribExcess :: Integral n => n -> n -> (n, n)
integralDistribExcess desired actual = (l, r)
  where
    excess =
      if desired > actual
      then desired - actual
      else 0
    l = excess `quot` 2
    r = excess - l

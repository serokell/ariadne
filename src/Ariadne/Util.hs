module Ariadne.Util where

import Prelude
import Control.Lens

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])

class HasReview s a where
    {-# MINIMAL reviewOf | inj #-}
    reviewOf :: Review s a
    reviewOf = unto inj

    inj :: a -> s
    inj = review reviewOf

class HasReview s a => HasPrism s a where
    {-# MINIMAL prismOf | proj #-}
    prismOf :: Prism' s a
    prismOf = prism' inj proj

    proj :: s -> Maybe a
    proj = preview prismOf

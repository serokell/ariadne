module Ariadne.AuxxBackend (createAuxxBackend) where

import Prelude
import Control.Concurrent (threadDelay)

import Ariadne.Face

-- TODO: @Hithroc
createAuxxBackend :: IO (AuxxFace, UiFace -> IO ())
createAuxxBackend = do
  return (AuxxFace (\_cmdId _expr -> return ()), \_uiFace -> threadDelay 3000000)

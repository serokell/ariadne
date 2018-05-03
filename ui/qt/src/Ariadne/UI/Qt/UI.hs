module Ariadne.UI.Qt.UI where

import Universum

import Formatting

import Graphics.UI.Qtah.Signal (Signal, connect_)

type UI w r = ReaderT w IO r

runUI :: UI w r -> w -> IO r
runUI = runReaderT

connectSignal :: w -> o -> Signal o (IO r) -> UI w r -> IO ()
connectSignal widget object signal handler =
  liftIO $ connect_ object signal $ runUI handler widget

spanFormat :: Format a (Text -> a)
spanFormat =
    "<pre style='\
        \font-family: Hack, \"Fira Code\", \"Source Code Pro\", \"DejaVu Sans Mono\", monospace;\
        \white-space: pre-wrap;'><span>" % stext % "</span></pre>"

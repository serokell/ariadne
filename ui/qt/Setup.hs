{-# OPTIONS_GHC -Wall -Wcompat -Werror #-}

import Distribution.PackageDescription
  (BuildInfo(cSources, extraLibs), HookedBuildInfo, emptyBuildInfo)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, withPrograms)
import Distribution.Simple.Program (Program, runDbProgram, simpleProgram)
import Distribution.Simple.Setup
  (ConfigFlags, configVerbosity, fromFlagOrDefault)
import Distribution.Simple.UserHooks
  (UserHooks(hookedPrograms, postConf, preBuild))
import Distribution.Verbosity (normal)

rccProgram :: Program
rccProgram = simpleProgram "rcc"

main :: IO ()
main = defaultMainWithHooks ariadneHooks

ariadneHooks :: UserHooks
ariadneHooks = simpleUserHooks
  { hookedPrograms = [rccProgram]
  , postConf = \args cf pd lbi -> do
      runRcc cf lbi
      postConf simpleUserHooks args cf pd lbi
  , preBuild = \_ _ -> addCxxFiles
  }

runRcc :: ConfigFlags -> LocalBuildInfo -> IO ()
runRcc configFlags localBuildInfo = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags
      programDb = withPrograms localBuildInfo

  runDbProgram verbosity rccProgram programDb $
    ["resources/ariadne-qt.qrc", "-o", "resources/ariadne-qt.cpp"]

addCxxFiles :: IO HookedBuildInfo
addCxxFiles = do
  return (Just emptyBuildInfo
    { cSources = ["resources/ariadne-qt.cpp"]
    , extraLibs = ["Qt5Core"]
    }, [])

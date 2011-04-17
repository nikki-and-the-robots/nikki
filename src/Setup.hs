{-# language CPP #-}

module Main where


import Distribution.Simple

#ifdef darwin_HOST_OS
import Setup.Darwin
#endif


main =
  defaultMainWithHooks nikkiUserHooks{instHook = installHook}

#ifdef darwin_HOST_OS
nikkiUserHooks = macUserHooks
#else
nikkiUserHooks = simpleUserHooks
#endif

-- | prevent cabal install
installHook :: a -> b -> c -> d -> IO ()
installHook _ _ _ _ = putStrLn "\"cabal install\" is not supported. The game compiled fine, though."

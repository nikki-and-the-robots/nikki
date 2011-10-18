{-# language CPP #-}

module Main where


import Distribution.Simple

#ifdef darwin_HOST_OS
import Distribution.Setup.Darwin
#endif


main =
  defaultMainWithHooks nikkiUserHooks

#ifdef darwin_HOST_OS
nikkiUserHooks = macUserHooks
#else
nikkiUserHooks = simpleUserHooks
#endif

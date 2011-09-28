{-# language ScopedTypeVariables, DeriveDataTypeable #-}

module Network.Client.Exceptions where


import Prelude hiding (catch)

import Data.Typeable

import Text.Logging

import Control.Exception

import Base

import Network.Client


networkTry :: Application -> Parent -> IO AppState -> IO AppState
networkTry app parent a =
    foldr (.) id handlers $ a
  where
    handlers =
        mk catchAll :
        mk errorCall :
        mk ioException :
        mk curlException :
        mk timeout :
        []
    mk :: (Show exception, Exception exception) =>
            (exception -> [Prose]) -> IO AppState -> IO AppState
    mk = mkHandler app parent

mkHandler :: forall exception . (Show exception, Exception exception) =>
    Application -> AppState -> (exception -> [Prose]) -> IO AppState -> IO AppState
mkHandler app follower mkMsgs action =
    catch action $ \ e -> do
        logg Warning $ show (e :: exception)
        return $ message app (mkMsgs e) follower

-- * exception types

catchAll :: SomeException -> [Prose]
catchAll (SomeException x) = [pv (show (typeOf x))]

timeout :: Timeout -> [Prose]
timeout t =
    p "The connection timed out." :
    []

data CurlException = CurlException String String
  deriving (Typeable, Show)

instance Exception CurlException

curlException :: CurlException -> [Prose]
curlException e@(CurlException url curlMsg) =
    p "An error occurred while downloading:" :
    pv url :
    pv ("(" ++ curlMsg ++ ")") :
    []

ioException :: IOException -> [Prose]
ioException e =
    p "SERVER ERROR:" :
    p "The level server seems to be down." :
    p "Sorry." :
    pv ("(" ++ show e ++ ")") :
    []

errorCall :: ErrorCall -> [Prose]
errorCall e =
    p "SERVER ERROR:" :
    p "The level server seems to be malfunctioning." :
    p "(Oh, my god!)" :
    p "Sorry." :
    p "Please, try updating your game." :
    pv ("(" ++ show e ++ ")") :
    []

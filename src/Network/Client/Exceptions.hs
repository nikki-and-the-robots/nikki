{-# language ScopedTypeVariables, DeriveDataTypeable #-}

module Network.Client.Exceptions where


import Prelude hiding (catch)

import Data.Typeable

import Text.Logging

import Control.Exception

import Network.Client

import Base


networkTry :: Application -> Parent -> IO AppState -> IO AppState
networkTry app parent a =
    foldr (.) id handlers $ a
  where
    handlers =
        mk catchAll :
        mk errorCall :
        mk ioException :
        mk downloadException :
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

data DownloadException = DownloadException String String
  deriving (Typeable, Show)

instance Exception DownloadException

downloadException :: DownloadException -> [Prose]
downloadException e@(DownloadException url errorMsg) =
    p "An error occurred while downloading:" :
    pv url :
    pv ("(" ++ errorMsg ++ ")") :
    []

ioException :: IOException -> [Prose]
ioException e =
    p "SERVER ERROR:" :
    p "The server seems to be down." :
    p "Sorry." :
    pv ("(" ++ show e ++ ")") :
    []

errorCall :: ErrorCall -> [Prose]
errorCall e =
    p "SERVER ERROR:" :
    p "The level server seems to be malfunctioning." :
    p "(Blimey!)" :
    p "Sorry." :
    p "Please, try updating your game." :
    pv ("(" ++ show e ++ ")") :
    []

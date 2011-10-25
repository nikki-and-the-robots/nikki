{-# language DeriveDataTypeable #-}

module LevelServer.SendMail where


import Data.Data

import Control.Monad
import Control.Exception

import System.Process
import System.Exit


-- | @sendMail address subject body@ sends an email with
-- the given body and subject to the given address.
sendMail :: String -> String -> String -> IO ()
sendMail address subject body = do
    (ec, _, stderr) <- readProcessWithExitCode "/usr/sbin/sendmail" [address] stdin
    when (ec /= ExitSuccess) $
        throwIO $ SendMailException stderr ec
    return ()
  where
    stdin = "subject: " ++ subject ++ "\n" ++ body

data SendMailException = SendMailException {
    stderr :: String,
    exitCode :: ExitCode
  }
    deriving (Show, Typeable)

instance Exception SendMailException

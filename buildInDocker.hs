#!/usr/bin/env stack
{- stack script --resolver=lts-17.11
--package base
--package filepath
--package shake
-}

{-# LANGUAGE ScopedTypeVariables #-}

import Development.Shake
import System.FilePath

tarFile = "Nikki-and-the-Robots.tar.gz"

distDir = "Nikki-and-the-Robots"

main :: IO ()
main = do
  unit $ cmd "rm" distDir tarFile "-rf"
  unit $ cmd "docker build -t nikki ."
  StdoutTrim (id :: String) <- cmd "docker create nikki"
  unit $ cmd "docker cp" (id ++ ":/root/nikki/src/nikki") distDir
  unit $ cmd "docker rm -v" id
  unit $ cmd (distDir </> "nikki.sh") "--help"
  unit $ cmd Shell "apack" tarFile (distDir </> "*")
  putStrLn "SUCCESS"

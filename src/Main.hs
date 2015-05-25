{-# LANGUAGE QuasiQuotes #-}
module Main where

import System.Environment
import Common.TH

manual :: String
manual = [str|
USAGE:

doctor <path to git repo/git url> <path to docs>
    The usual usage. Builds the documentation connecting the references to the specified repository.

doctor /help
    Shows this page
|]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["/help"]   -> putStrLn manual
        [git, docs] -> return ()
        _           -> putStrLn "Expected two command line arguments. Try /help."

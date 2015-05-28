{-# LANGUAGE QuasiQuotes #-}
module Main where

import Common.Prelude
import System.Environment
import System.FilePath ((</>))
import qualified System.Directory as Dir
import Common.TH
import qualified Parsing.Docs as Docs
import Control.Applicative
import Control.Monad

manual :: String
manual = [str|
    USAGE:

    doctor <path to git repo/git url> <path to docs>
        The usual usage. Builds the documentation connecting the references to the specified repository.

    doctor /help
        Shows this page |]

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = relative >>= mapM (Dir.canonicalizePath . (dir </>))
    where relative = filter (`notElem` [".", ".."]) <$> Dir.getDirectoryContents dir

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["/help"]   -> putStrLn manual
        [_, docs] -> do
            files <- getAbsDirectoryContents docs
            forM_ files $ \f -> print =<< Docs.fromString <$> readFile f
        _           -> putStrLn "Expected two command line arguments. Try /help."

{-# LANGUAGE ViewPatterns #-}
module Providers.Line.Main where

import Common.Prelude
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file, read->start, read->end] -> do
            src <- readFile file
            print start
            print end
            putStrLn . unlines . take (end - start) . drop (start - 1) . lines $ src
        _ -> error "Expected usage: doctor-line <file> <start> <end>"

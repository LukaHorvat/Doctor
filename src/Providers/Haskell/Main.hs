module Providers.Haskell.Main where

import System.Environment
import Data.List
import Data.Function
import Control.Arrow ((>>>))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file, identifier] -> do
            src <- readFile file
            putStrLn $ findIdent src identifier
        _ -> error "Expected usage: doctor-haskell <file path> <identifier>"
    return ()

trim :: String -> String
trim = reverse >>> dropWhile (`elem` " \t\n") >>> reverse

findIdent :: String -> String -> String
findIdent src ident =
    case find (firstWord ident) parts of
        Nothing -> ""
        Just m  -> trim m
    where parts = map concat . groupBy firstWord . map unlines . split . lines $ src
          split []       = []
          split (x : xs) = let (left, rest) = break noIndent xs in (x : left) : split rest
          noIndent ""      = False
          noIndent (x : _) = x `notElem` " \t"
          firstWord = (==) `on` takeWhile (`notElem` " :=")

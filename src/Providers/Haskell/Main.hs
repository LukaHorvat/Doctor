module Providers.Haskell.Main where

import Common.Prelude
import System.Environment
import Data.List
import Data.Function

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file, identifier] -> do
            src <- readFile file
            let (start, end, str) = findIdent src identifier
            print start
            print end
            putStrLn str
        _ -> error "Expected usage: doctor-haskell <file path> <identifier>"
    return ()

trim :: (Int, Int, String) -> (Int, Int, String)
trim (start, end, inp) = (start, end - newLines + 1, reverse . dropWhile cond $ rev)
    where rev = reverse inp
          newLines = length . filter (== '\n') . takeWhile cond $ rev
          cond = (`elem` " \t\n")

findIdent :: String -> String -> (Int, Int, String)
findIdent src ident =
    case find ((== ident) . firstWord . trd) merged of
        Nothing -> (0, 0, "")
        Just m  -> trim m
    where merged = map mergeParts . groupBy ((==) `on` (firstWord . trd)) $ parts
          parts = map makePart . split . zip [1..] . lines $ src
          split []       = []
          split (x : xs) = let (left, rest) = break (noIndent . snd) xs in (x : left) : split rest
          noIndent ""      = False
          noIndent (x : _) = x `notElem` " \t"
          firstWord = takeWhile (`notElem` " :=")
          makePart ls = let ns = map fst ls in (minimum ns, maximum ns, unlines $ map snd ls)
          mergeParts ps = (fst . head $ ps, snd . last $ ps, concat . map trd $ ps)

module Providers.Haskell.Main where

import Common.Prelude
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    (start, end, str) <- case args of
        [file, ""] -> do
            src <- readFile file
            let ln =  length (lines src) + 1
            return (1, ln, src)
        [file, identifier] -> do
            src <- readFile file
            return $ findIdent src identifier
        _ -> error "Expected usage: doctor-haskell <file path> <identifier|\"\">"
    print start
    print end
    putStrLn str

trim :: (Int, Int, String) -> (Int, Int, String)
trim (start, end, inp) = (start, end - newLines + 1, reverse . dropWhile cond $ rev)
    where rev = reverse inp
          newLines = length . filter (== '\n') . takeWhile cond $ rev
          cond = (`elem` " \t\n")

findIdent :: String -> String -> (Int, Int, String)
findIdent src ident =
    case find ((`startsWith` ident) . trd) merged of
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
          mergeParts ps = (fst . head $ ps, snd . last $ ps, concatMap trd ps)

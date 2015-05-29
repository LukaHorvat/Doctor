{-# LANGUAGE QuasiQuotes #-}
module Main where

import Common.Prelude
import System.Environment
import System.FilePath ((</>))
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import Common.TH
import qualified Parsing.Docs as Docs
import qualified Render.Compile as Comp
import qualified Render.Html as Html

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

isPhd :: FilePath -> Bool
isPhd = (== ".md") . FP.takeExtension

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["/help"]   -> putStrLn manual
        [prov, docs] -> do
            files <- getAbsDirectoryContents docs
            forM_ (filter isPhd files) $ \f -> do
                eith <- Docs.fromString <$> readFile f
                let phdOut = FP.dropExtension f `FP.addExtension` ".phd"
                let htmlOut = FP.dropExtension f `FP.addExtension` ".html"
                case eith of
                    Left err  -> print err
                    Right doc -> do
                        phd <- Comp.compile prov doc
                        Comp.phdToFile phdOut phd
                        writeFile htmlOut $ Html.render doc phd
        _           -> putStrLn "Expected two command line arguments. Try /help."

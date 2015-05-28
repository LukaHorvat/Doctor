{-# LANGUAGE TupleSections, TemplateHaskell #-}
-- Compiles documentation files into .phd
module Render.Compile where

import Common.Prelude
import Parsing.Docs (Ref(..), Doc(..), Chunk(..), refToString)
import Data.Map (Map)
import Data.Aeson.TH
import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified System.Process as Proc
import qualified Data.ByteString.Lazy as BS

data Phd = Phd (Map String [Snippet]) deriving (Eq, Ord, Show, Read)
data Snippet = Snippet { snippetString :: String
                       , snippetStart  :: Int
                       , snippetEnd    :: Int
                       , snippetFile   :: String
                       , snippetCommit :: String } deriving (Eq, Ord, Show, Read)

deriveJSON defaultOptions ''Phd
deriveJSON defaultOptions ''Snippet

type DefaultProvider = String

compile :: DefaultProvider -> Doc -> IO Phd
compile prov (Doc chunks) = do
    list <- mapM (\ref -> (refToString ref,) <$> snippets ref) refs
    return $ Phd $ Map.fromList list
    where refs = [setProvider ref | Tagged _ ref <- chunks]
          setProvider ref
              | refProvider ref == "" = ref { refProvider = prov }
              | otherwise             = ref

snippets :: Ref -> IO [Snippet]
snippets (Ref prov file markers) = mapM (snippet prov file) markers

snippet :: String -> String -> String -> IO Snippet
snippet prov file marker = do
    res <- Proc.readProcess prov [file, marker] ""
    case lines res of
        []  -> error $ "Provider " ++ prov ++ " could not find reference " ++ file ++ " " ++ marker
        start : end : rest -> return $ Snippet (unlines rest) (read start) (read end) file ""
        [what] -> error $ "Unrecognized output from provider" ++ what

phdToFile :: FilePath -> Phd -> IO ()
phdToFile path (Phd mp) = BS.writeFile path $ Json.encode mp

{-# LANGUAGE TupleSections, TemplateHaskell, RecordWildCards #-}
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
data Snippet = Snippet { snippetString   :: String
                       , snippetStart    :: Int
                       , snippetEnd      :: Int
                       , snippetCommit   :: String
                       , snippetInternal :: Bool
                       , snippetRefId    :: RefId } deriving (Eq, Ord, Show, Read)
data RefId = RefId { refIdProvider :: String
                   , refIdFile     :: String
                   , refIdMarker   :: String } deriving (Read, Show, Eq, Ord)

deriveJSON defaultOptions ''RefId
deriveJSON defaultOptions ''Phd
deriveJSON defaultOptions ''Snippet

type DefaultProvider = String

showRefId :: RefId -> String
showRefId RefId{..} = show $ unwords [refIdProvider, refIdFile, refIdMarker]

getRefIds :: Ref -> [RefId]
getRefIds Ref{..} = map (RefId refProvider refFile) refMarkers

compile :: DefaultProvider -> Doc -> IO (Doc, Phd)
compile prov (Doc chunks) = do
    list <- mapM (\ref -> (refToString ref,) <$> snippets ref) refs
    return (Doc providerChunks, Phd $ Map.fromList list)
    where providerChunks = map setProvider chunks
          refs = [ref | Tagged _ ref <- providerChunks]
          setProvider (Tagged s ref)
              | refProvider ref == "" = Tagged s $ ref { refProvider = prov }
              | otherwise             = Tagged s ref
          setProvider x = x

snippets :: Ref -> IO [Snippet]
snippets ref = mapM (snippet $ refInternal ref) $ getRefIds ref

snippet :: Bool -> RefId -> IO Snippet
snippet internal rid@RefId{ refIdProvider = prov, refIdFile = file, refIdMarker = marker } = do
    res <- Proc.readProcess prov [file, marker] ""
    case lines res of
        []  -> error $ "Provider " ++ prov ++ " could not find reference " ++ file ++ " " ++ marker
        start : end : rest -> return $ Snippet (unlines rest) (read start) (read end) "" internal rid
        [what] -> error $ "Unrecognized output from provider" ++ what

phdToFile :: FilePath -> Phd -> IO ()
phdToFile path (Phd mp) = BS.writeFile path $ Json.encode mp

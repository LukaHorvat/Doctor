{-# LANGUAGE TupleSections, TemplateHaskell, RecordWildCards #-}
-- Compiles documentation files into .phd
module Render.Compile where

import Common.Prelude
import Parsing.Docs (Ref(..), Doc(..), Chunk(..), Command(..), refToString)
import Data.Map (Map)
import Data.Aeson.TH
import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified System.Process as Proc
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State (State)
import qualified Control.Monad.State as State

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

data Defaults = Defaults { defaultsProvider :: String
                         , defaultsFile     :: String } deriving (Ord, Eq, Show, Read)

type DefaultProvider = String

showRefId :: RefId -> String
showRefId RefId{..} = show $ unwords [refIdProvider, refIdFile, refIdMarker]

getRefIds :: Ref -> [RefId]
getRefIds Ref{..} = map (RefId refProvider refFile) refMarkers

compile :: DefaultProvider -> Doc -> IO (Doc, Phd)
compile prov (Doc chunks) = do
    list <- mapM (\ref -> (refToString ref,) <$> snippets ref) refs
    return (Doc taggedChunks, Phd $ Map.fromList list)
    where tag :: Chunk -> State Defaults (Maybe Chunk)
          tag (Tagged str ref@Ref{..} ) = State.get >>= \defs -> return . Just . Tagged str $ ref {
              refProvider = changeIf null (defaultsProvider defs) refProvider,
              refFile     = changeIf null (defaultsFile     defs) refFile }
          tag (Command (ChangeFile file)) = State.modify (\def -> def { defaultsFile = file })
                                         >> return Nothing
          tag (Command (ChangeProvider p)) = State.modify (\def -> def { defaultsProvider = p })
                                          >> return Nothing
          tag x = return . Just $ x
          taggedChunks = catMaybes $ State.evalState (mapM tag chunks) (Defaults prov "")
          refs = [ref | Tagged _ ref <- taggedChunks]

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

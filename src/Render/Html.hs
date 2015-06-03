{-# LANGUAGE FlexibleInstances, FlexibleContexts, RecordWildCards, TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Render.Html where

import Common.Prelude hiding (div, span)
import Common.TH (embedFile)
import Parsing.Docs (Doc(..), Chunk(..), Ref(..))
import qualified Parsing.Docs as Docs
import Render.Compile (Phd(..), Snippet(..), RefId(..))
import qualified Render.Compile as Comp
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as Read

baseCss :: String
baseCss = $(embedFile "./src/Resources/base.css")

baseJs :: String
baseJs = $(embedFile "./src/Resources/base.js")

type Attribute = (String, String)

type Html = String

class Decorator f where
    tag :: String -> f

instance Decorator ([Attribute] -> Html -> Html) where
    tag name attrs inner = "<" ++ name ++ concatMap showAtt attrs ++ ">" ++ inner ++ "</" ++ name ++ ">"
        where showAtt (attName, value) = " " ++ attName ++ "=\"" ++ escape value ++ "\""

instance Decorator (Html -> Html) where
    tag name inner = tag name ([] :: [Attribute]) inner

instance Decorator (String -> Html -> Html) where
    tag name cls inner = tag name [("class", cls)] inner

div :: Decorator f => f
div = tag "div"

p :: Decorator f => f
p = tag "p"

span :: Decorator f => f
span = tag "span"

pre :: Decorator f => f
pre = tag "pre"

style :: Decorator f => f
style = tag "style"

script :: Decorator f => f
script = tag "script"

render :: Doc -> Phd -> Html
render (Doc chunks) (Phd mp) = script baseJs ++ style baseCss ++ div "container" inner
    where inner :: String
          inner = div "prose" (concatMap (plainChunk lookupTable) chunks)
               ++ div "code"  (concat $ flip Read.runReader mp $ mapM (refChunk lookupTable) chunks)
          refIds = concat [Comp.getRefIds r | Tagged _ r <- chunks]
          lookupTable = fst $ foldl maybeInsert (Map.empty, 0) refIds
          maybeInsert (m, c) r | Map.member r m = (m, c)
                               | otherwise      = (Map.insert r c m, c + 1)

plainChunk :: Map RefId Int -> Chunk -> String
plainChunk _  (Untagged s)   = escape s
plainChunk mp (Tagged s ref) = span attrs $ escape s
    where internal | Docs.refInternal ref = "code-ref-internal"
                   | otherwise            = "code-ref"
          attrs = [("class", internal), ("data-ref-id", ids)
                  ,("onmouseover", "hover(this)"), ("onmouseout", "unhover(this)")]
          ids = intercalate "," . map (\rid -> show $ mp Map.! rid) . Comp.getRefIds $ ref

refChunk :: Map RefId Int -> Chunk -> Reader (Map String [Snippet]) String
refChunk _  (Untagged _)   = return ""
refChunk mp (Tagged _ ref) = maybe "" (toCodeHtml mp) <$> Read.asks (Map.lookup $ Docs.refToString ref)

fileGroups :: [Snippet] -> [[Snippet]]
fileGroups = groupBy ((==) `on` (Comp.refIdFile . Comp.snippetRefId))

toCodeHtml :: Map RefId Int -> [Snippet] -> String
toCodeHtml mp snips = concat [wrap snippetRefId $ span (Comp.refIdFile snippetRefId) ++ pre (escape snippetString)
                          | Snippet{ Comp.snippetInternal = False, ..} <- snips]
    where wrap :: RefId -> String -> String
          wrap rid = div ("ref-id-" ++ show (mp Map.! rid))

escape :: String -> String
escape str = replace "<" "&lt;"
           . replace ">" "&gt;"
           . replace "\"" "&quot;"
           . replace "&" "&amp;"
           $ str

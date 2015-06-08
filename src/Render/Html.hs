{-# LANGUAGE FlexibleInstances, FlexibleContexts, RecordWildCards, TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Render.Html where

import Common.Prelude hiding (div, span)
import Common.TH (embedFile)
import Parsing.Docs (Doc(..), Chunk(..))
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

marked :: String
marked = $(embedFile "./src/Resources/marked.js")

type Attribute = (String, String)

type Html = String

class Decorator f where
    tag :: String -> f

instance Decorator ([Attribute] -> Html -> Html) where
    tag name attrs inner = "<" ++ name ++ concatMap showAtt attrs ++ ">" ++ inner ++ "</" ++ name ++ ">"
        where showAtt (attName, value) = " " ++ attName ++ "=\"" ++ escape value ++ "\""

instance Decorator (Html -> Html) where
    tag name = tag name ([] :: [Attribute])

instance Decorator (String -> Html -> Html) where
    tag name cls = tag name [("class", cls)]

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

body :: Decorator f => f
body = tag "body"

br :: Decorator f => f
br = tag "br"

meta :: Decorator f => f
meta = tag "meta"

render :: Doc -> Phd -> Html
render (Doc chunks) (Phd mp) = script marked ++ script baseJs ++ style baseCss ++ charset ++ body'
    where inner :: String
          inner = div "prose" (div "prose-text" (concatMap (plainChunk lookupTable) chunks) :: String)
               ++ div "code"  (concat $ flip Read.runReader mp $ mapM (refChunk lookupTable) chunks)
          refIds = concat [Comp.getRefIds r | Tagged _ r <- chunks]
          lookupTable = fst $ foldl maybeInsert (Map.empty, 0) refIds
          maybeInsert (m, c) r | Map.member r m = (m, c)
                               | otherwise      = (Map.insert r c m, c + 1)
          body' = body (div "container" inner :: String)
          charset = meta [("charset", "UTF-8")] ""

dropString :: String -> String -> Maybe String
dropString str text = if text `startsWith` str then Just $ drop (length str) text else Nothing

insertBreaks :: String -> String
insertBreaks ('\\' : c : rest) = '\\' : c : insertBreaks rest
insertBreaks str | isBreak = p "doc-break" "" ++ insertBreaks afterBreak
    where isBreak = isJust afterBreakM
          afterBreak = fromJust afterBreakM
          afterBreakM = dropString "-)"
                    =<< return . dropWhile (== ' ')
                    =<< dropString "break"
                    =<< return . dropWhile (== ' ')
                    =<< dropString "(-"
                    =<< Just str

insertBreaks (c : cs) = c : insertBreaks cs
insertBreaks "" = ""

plainChunk :: Map RefId Int -> Chunk -> String
plainChunk _  (Untagged s)   = insertBreaks $ escape s
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
toCodeHtml mp snips = concat [wrap snippetRefId $ span "file-name" (Comp.refIdFile snippetRefId) ++ pre (escape snippetString)
                             | Snippet{ Comp.snippetInternal = False, ..} <- snips]
    where wrap :: RefId -> String -> String
          wrap rid = div $ "code-snippet " ++ "ref-id-" ++ show (mp Map.! rid)

escape :: String -> String
escape str = replace "<" "&lt;"
           . replace ">" "&gt;"
           . replace "\"" "&quot;"
           . replace "&" "&amp;"
           $ str

{-# LANGUAGE FlexibleInstances, FlexibleContexts, RecordWildCards, TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Render.Html where

import Common.Prelude hiding (div, span)
import Common.TH (embedFile)
import Parsing.Docs (Doc(..), Chunk(..))
import qualified Parsing.Docs as Docs
import Render.Compile (Phd(..), Snippet(..))
import qualified Render.Compile as Comp
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as Read

baseCss :: String
baseCss = $(embedFile "./src/Resources/base.css")

type Attribute = (String, String)

class Decorator f where
    tag :: String -> f

instance Decorator ([Attribute] -> Html -> Html) where
    tag name attrs inner = "<" ++ name ++ concatMap showAtt attrs ++ ">" ++ inner ++ "</" ++ name ++ ">"
        where showAtt (attName, value) = " " ++ attName ++ "=" ++ show value

instance Decorator (Html -> Html) where
    tag name inner = tag name ([] :: [Attribute]) inner

instance Decorator (String -> Html -> Html) where
    tag name cls inner = tag name [("class", cls)] inner

type Html = String

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

render :: Doc -> Phd -> Html
render (Doc chunks) (Phd mp) = style baseCss ++ div "container" inner
    where inner :: String
          inner = div "prose" (concatMap plainChunk chunks)
               ++ div "code"  (concat $ flip Read.runReader mp $ mapM refChunk chunks)

plainChunk :: Chunk -> String
plainChunk (Untagged s) = escape s
plainChunk (Tagged s ref) = span attrs $ escape s
    where internal | Docs.refInternal ref = "code-ref-internal"
                   | otherwise            = "code-ref"
          attrs = [("class", internal), ("data-ref-target", Docs.refToString ref)]

refChunk :: Chunk -> Reader (Map String [Snippet]) String
refChunk (Untagged _) = return ""
refChunk (Tagged _ ref) = maybe "" toCodeHtml <$> Read.asks (Map.lookup (trace $ Docs.refToString ref))

fileGroups :: [Snippet] -> [[Snippet]]
fileGroups = groupBy ((==) `on` Comp.snippetFile)

toCodeHtml :: [Snippet] -> String
toCodeHtml snips = concat [span snippetFile ++ pre (escape snippetString) | Snippet{ Comp.snippetInternal = False, ..} <- snips]

escape :: String -> String
escape str = replace "<" "&lt;" . replace ">" "&gt;" . replace "&" "&amp;" $ str

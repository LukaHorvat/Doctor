{-# LANGUAGE FlexibleInstances #-}
module Render.Html where

import Common.Prelude hiding (div)
import Parsing.Docs (Doc(..), Chunk(..), Ref(..))
import Render.Compile (Phd(..))
import qualified Render.Compile as Comp
import Data.Map (Map)
import qualified Data.Map as Map

class Decorator f where
    tag :: String -> f

instance Decorator (Html -> Html) where
    tag name inner = "<" ++ name ++ ">" ++ inner ++ "</" ++ name ++ ">"

instance Decorator (String -> Html -> Html) where
    tag name cls inner = "<" ++ name ++ " class=\"" ++ cls ++ "\">" ++ inner ++ "</" ++ name ++ ">"

type Html = String

div :: Decorator f => f
div = tag "div"

p :: Decorator f => f
p = tag "p"

render :: Doc -> Phd -> Html
render (Doc chunks) (Phd mp) = div "prose" (concatMap plainChunk chunks)

plainChunk :: Chunk -> String
plainChunk (Untagged s) = s
plainChunk (Tagged s _) = s

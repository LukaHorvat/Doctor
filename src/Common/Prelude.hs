{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Common.Prelude ( module Prelude
                      , module Common.Prelude
                      , module Control.Applicative
                      , module Data.List
                      , module Data.Function
                      , module Data.Ord
                      , module Data.Char
                      , module Control.Monad ) where

import Prelude hiding (fst, snd)
import Common.PreludeTH
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function
import Data.Ord
import Data.Char
import Debug.Trace hiding (trace)

generateTupleAccessors ["fst", "snd", "trd", "frth", "ffth"]

trace :: Show a => a -> a
trace = traceShowId

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new target = replace' target
    where len = length old
          replace' [] = []
          replace' tgt | take len tgt == old = new ++ replace' (drop len tgt)
          replace' (x : xs) = x : replace' xs

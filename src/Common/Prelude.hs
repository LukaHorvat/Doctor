{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Common.Prelude ( module Prelude
                      , module Common.Prelude
                      , module Control.Applicative
                      , module Control.Monad ) where

import Prelude hiding (fst, snd)
import Common.PreludeTH
import Control.Applicative
import Control.Monad

generateTupleAccessors ["fst", "snd", "trd", "frth", "ffth"]

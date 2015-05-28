{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Common.Prelude ( module Prelude
                      , module Common.Prelude ) where

import Prelude hiding (fst, snd)
import Common.PreludeTH

generateTupleAccessors ["fst", "snd", "trd", "frth", "ffth"]

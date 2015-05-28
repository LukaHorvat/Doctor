{-# LANGUAGE TemplateHaskell #-}
module Common.TH where

import Common.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec = undefined }

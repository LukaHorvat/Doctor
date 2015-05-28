{-# LANGUAGE TemplateHaskell #-}
module Common.PreludeTH where

import Language.Haskell.TH
import Data.Char
import Control.Monad
import Prelude

generateTupleAccessors :: [String] -> Q [Dec]
generateTupleAccessors names = do
    classes <- mapM makeClass names
    let instances = [makeInstance tpl i cls | (i, cls) <- zip [1..] names, tpl <- [max i 2..length names]]
    return $ classes ++ instances
    where capName ~(c : cs) = toUpper c : cs
          makeClass name =
              classD (return []) (mkName $ capName name)
                     [PlainTV $ mkName "t", PlainTV $ mkName "a"]
                     [FunDep [mkName "t"] [mkName "a"]]
                     [sigD (mkName name) [t|$(varT $ mkName "t") -> $(varT $ mkName "a")|] ]
          makeInstance tpl i name =
              InstanceD [] (AppT (AppT (ConT $ mkName $ capName name) tupleType) selectedType)
                        [FunD (mkName name) [Clause [tuplePat] (NormalB selectedVar) []]]
              where tuple = map (\n -> mkName $ "a" ++ show n) [1..tpl]
                    tupleType = foldl AppT (TupleT $ tpl) $ map VarT tuple
                    selectedType = VarT $ mkName $ "a" ++ show i
                    tuplePat = TupP $ map VarP tuple
                    selectedVar = VarE $ mkName $ "a" ++ show i

{-# language DataKinds, LambdaCase, ViewPatterns #-}
module Language.Python.Internal.Prisms where

import Control.Lens
import Data.Coerce
import Language.Python.Internal.Syntax

_KeywordParam
  :: Prism
       (Param v a)
       (Param '[] a)
       (a, String, Expr v a)
       (a, String, Expr '[] a)
_KeywordParam =
  prism
    (\(a, b, c) -> KeywordParam a b c)
    (\case; (coerce -> KeywordParam a b c) -> Right (a, b, c); (coerce -> a) -> Left a)

_Fundef
  :: Prism
       (Statement v a)
       (Statement '[] a)
       (a, String, Params v a, [Statement v a])
       (a, String, Params '[] a, [Statement '[] a])
_Fundef =
  prism
    (\(a, b, c, d) -> Fundef a b c d)
    (\case; (coerce -> Fundef a b c d) -> Right (a, b, c, d); (coerce -> a) -> Left a)

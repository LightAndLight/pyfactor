{-# language DataKinds, LambdaCase, ViewPatterns #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Optics where

import Control.Lens
import Data.Coerce
import Language.Python.Internal.Syntax

data KeywordParam v a
  = MkKeywordParam
  { _kpAnn :: a
  , _kpName :: String
  , _kpExpr :: Expr v a
  } deriving (Eq, Show)
makeLenses ''KeywordParam

_KeywordParam
  :: Prism
       (Param v a)
       (Param '[] a)
       (KeywordParam v a)
       (KeywordParam '[] a)
_KeywordParam =
  prism
    (\(MkKeywordParam a b c) -> KeywordParam a b c)
    (\case
        (coerce -> KeywordParam a b c) -> Right (MkKeywordParam a b c)
        (coerce -> a) -> Left a)

_Fundef
  :: Prism
       (Statement v a)
       (Statement '[] a)
       (a, String, Params v a, Block v a)
       (a, String, Params '[] a, Block '[] a)
_Fundef =
  prism
    (\(a, b, c, d) -> Fundef a b c d)
    (\case; (coerce -> Fundef a b c d) -> Right (a, b, c, d); (coerce -> a) -> Left a)

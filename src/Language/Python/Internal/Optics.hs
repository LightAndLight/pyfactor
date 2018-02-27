{-# language DataKinds, LambdaCase, ViewPatterns #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Optics where

import Control.Lens
import Data.Coerce
import Language.Python.Internal.Syntax

class Validated s where
  unvalidated :: Getter (s v a) (s '[] a)

instance Validated Expr where
  unvalidated = to coerce

instance Validated Statement where
  unvalidated = to coerce

instance Validated Block where
  unvalidated = to coerce

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

_Call
  :: Prism
       (Expr v a)
       (Expr '[] a)
       (a, Expr v a, Args v a)
       (a, Expr '[] a, Args '[] a)
_Call =
  prism
    (\(a, b, c) -> Call a b c)
    (\case; (coerce -> Call a b c) -> Right (a, b, c); (coerce -> a) -> Left a)

_Ident
  :: Prism
       (Expr v a)
       (Expr '[] a)
       (a, String)
       (a, String)
_Ident =
  prism
    (uncurry Ident)
    (\case; (coerce -> Ident a b) -> Right (a, b); (coerce -> a) -> Left a)

_Indents
  :: Traversal'
       (Statement v a)
       [Whitespace]
_Indents f = fmap coerce . (_Blocks._Wrapped) ((traverse._2) f . coerce)

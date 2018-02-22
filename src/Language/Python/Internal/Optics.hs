{-# language DataKinds, LambdaCase, ViewPatterns #-}
module Language.Python.Internal.Optics where

import Control.Lens
import Data.Coerce
import Language.Python.Internal.Syntax

-- | 'Traversal' over all the expressions in a term
class HasExprs s where
  _Exprs :: Traversal (s v a) (s '[] a) (Expr v a) (Expr '[] a)

instance HasExprs Param where
  _Exprs f (KeywordParam a name expr) = KeywordParam a name <$> f expr
  _Exprs _ p = pure $ coerce p

instance HasExprs Statement where
  _Exprs f (Fundef a name params sts) =
    Fundef a name <$>
    (traverse._Exprs) f params <*>
    (traverse._3._Exprs) f sts
  _Exprs f (Return a e) = Return a <$> f e
  _Exprs f (Expr a e) = Expr a <$> f e
  _Exprs f (If a e sts) = If a <$> f e <*> (traverse._3._Exprs) f sts
  _Exprs f (Assign a e1 e2) = Assign a <$> f e1 <*> f e2
  _Exprs _ p@Pass{} = pure $ coerce p

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
       (a, String, Params v a, [(a, [Whitespace], Statement v a)])
       (a, String, Params '[] a, [(a, [Whitespace], Statement '[] a)])
_Fundef =
  prism
    (\(a, b, c, d) -> Fundef a b c d)
    (\case; (coerce -> Fundef a b c d) -> Right (a, b, c, d); (coerce -> a) -> Left a)

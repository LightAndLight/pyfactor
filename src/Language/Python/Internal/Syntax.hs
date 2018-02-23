{-# language DeriveFunctor #-}
{-# language DataKinds, PolyKinds #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances,
  MultiParamTypeClasses #-}
module Language.Python.Internal.Syntax where

import Control.Lens.Getter
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Plated
import Control.Lens.Traversal
import Control.Lens.Wrapped
import Data.Coerce
import Data.Functor
import Data.Monoid
import Data.String

type Params v a = [Param v a]
data Param (v :: [*]) a
  = PositionalParam a String
  | KeywordParam a String (Expr v a)
  deriving (Eq, Show)

data Args (v :: [*]) a
  = NoArgs a
  | PositionalArg a (Expr v a) (Args v a)
  deriving (Eq, Show)
instance HasExprs Args where
  _Exprs _ (NoArgs a) = pure $ NoArgs a
  _Exprs f (PositionalArg a expr args) = PositionalArg a <$> f expr <*> _Exprs f args

data Whitespace = Space | Tab | Continued [Whitespace] deriving (Eq, Show)

newtype Block v a = Block [(a, [Whitespace], Statement v a)]
  deriving (Eq, Show)

data Statement (v :: [*]) a
  = Fundef a String (Params v a) (Block v a)
  | Return a (Expr v a)
  | Expr a (Expr v a)
  | If a (Expr v a) (Block v a)
  | Assign a (Expr v a) (Expr v a)
  | Pass a
  deriving (Eq, Show)
instance Plated (Statement v a) where
  plate f (Fundef a b c sts) = Fundef a b c <$> (_Wrapped.traverse._3) f sts
  plate f (If a b sts) = If a b <$> (_Wrapped.traverse._3) f sts
  plate _ p = pure p

data Expr (v :: [*]) a
  = List a [Expr v a]
  | Deref a (Expr v a) String
  | Call a (Expr v a) (Args v a)
  | None a
  | BinOp a (BinOp a) (Expr v a) (Expr v a)
  | Ident a String
  | Int a Integer
  deriving (Eq, Show)
instance IsString (Expr '[] ()) where
  fromString = Ident ()
instance Num (Expr '[] ()) where
  fromInteger = Int ()
  negate = undefined
  (+) = undefined
  (*) = undefined
  (-) = undefined
  signum = undefined
  abs = undefined
instance Plated (Expr '[] ()) where
  plate f (List a exprs) = List a <$> traverse f exprs
  plate f (Deref a expr name) = Deref a <$> f expr <*> pure name
  plate f (Call a expr args) = Call a <$> f expr <*> _Exprs f args
  plate _ (None a) = pure $ None a
  plate f (BinOp a op e1 e2) = BinOp a op <$> f e1 <*> f e2
  plate f (Ident a name) = pure $ Ident a name
  plate _ (Int a n) = pure $ Int a n

data BinOp a
  = Is a
  | Minus a
  | Exp a
  deriving (Eq, Show, Functor)

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
    (_Wrapped.traverse._3._Exprs) f sts
  _Exprs f (Return a e) = Return a <$> f e
  _Exprs f (Expr a e) = Expr a <$> f e
  _Exprs f (If a e sts) = If a <$> f e <*> (_Wrapped.traverse._3._Exprs) f sts
  _Exprs f (Assign a e1 e2) = Assign a <$> f e1 <*> f e2
  _Exprs _ p@Pass{} = pure $ coerce p

-- | 'Traversal' over all the statements in a term
class HasStatements s where
  _Statements :: Traversal (s v a) (s '[] a) (Statement v a) (Statement '[] a)

instance HasStatements Block where
  _Statements = _Wrapped.traverse._3

data Assoc = L | R
data OpEntry
  = OpEntry
  { _opOperator :: BinOp ()
  , _opPrec :: Int
  , _opAssoc :: Assoc
  }
makeLenses ''OpEntry

operatorTable :: [OpEntry]
operatorTable =
  [ entry Is 10 L
  , entry Minus 20 L
  , entry Exp 30 R
  ]
  where
    entry a = OpEntry (a ())

lookupOpEntry :: BinOp a -> [OpEntry] -> OpEntry
lookupOpEntry op =
  go (op $> ())
  where
    go op [] = error $ show op <> " not found in operator table"
    go op (x:xs)
      | x ^. opOperator == op = x
      | otherwise = go op xs

makeWrapped ''Block

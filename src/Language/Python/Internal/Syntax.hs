{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
class HasBlocks s where
  _Blocks :: Traversal (s v a) (s '[] a) (Block v a) (Block '[] a)
instance HasBlocks Statement where
  _Blocks f (Fundef a name params b) = Fundef a name (coerce params) <$> coerce (f b)
  _Blocks _ (Return a expr) = pure $ Return a (coerce expr)
  _Blocks _ (Expr a expr) = pure $ Expr a (coerce expr)
  _Blocks f (If a e1 b) = If a (coerce e1) <$> coerce (f b)
  _Blocks _ (Assign a e1 e2) = pure $ Assign a (coerce e1) (coerce e2)
  _Blocks _ (Pass a) = pure $ Pass a

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

data CommaSep a
  = CommaSepNone
  | CommaSepOne a (Maybe [Whitespace])
  | CommaSepMany a [Whitespace] [Whitespace] (CommaSep a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
listToCommaSep :: [a] -> CommaSep a
listToCommaSep [] = CommaSepNone
listToCommaSep [a] = CommaSepOne a Nothing
listToCommaSep (a:as) = CommaSepMany a [] [] $ listToCommaSep as

data Expr (v :: [*]) a
  = List a [Whitespace] (CommaSep (Expr v a)) [Whitespace]
  | Deref a (Expr v a) [Whitespace] [Whitespace] String
  | Call a (Expr v a) [Whitespace] (Args v a)
  | None a
  | BinOp a (Expr v a) [Whitespace] (BinOp a) [Whitespace] (Expr v a)
  | Negate a [Whitespace] (Expr v a)
  | Parens a [Whitespace] (Expr v a) [Whitespace]
  | Ident a String
  | Int a Integer
  | Bool a Bool
  deriving (Eq, Show)
instance IsString (Expr '[] ()) where
  fromString = Ident ()
instance Num (Expr '[] ()) where
  fromInteger = Int ()
  negate = Negate () []
  (+) a = BinOp () a [Space] (Plus ()) [Space]
  (*) a = BinOp () a [Space] (Multiply ()) [Space]
  (-) a = BinOp () a [Space] (Minus ()) [Space]
  signum = undefined
  abs = undefined
instance Plated (Expr '[] ()) where
  plate f (Parens a ws1 e ws2) = Parens a ws1 <$> f e <*> pure ws2
  plate _ (Bool a b) = pure $ Bool a b
  plate f (List a ws1 exprs ws2) = List a ws1 <$> traverse f exprs <*> pure ws2
  plate f (Deref a expr ws1 ws2 name) =
    Deref a <$> f expr <*> pure ws1 <*> pure ws2 <*> pure name
  plate f (Call a expr ws args) = Call a <$> f expr <*> pure ws <*> _Exprs f args
  plate _ (None a) = pure $ None a
  plate f (BinOp a e1 ws1 op ws2 e2) =
    (\e1' e2' -> BinOp a e1' ws1 op ws2 e2') <$> f e1 <*> f e2
  plate _ (Ident a name) = pure $ Ident a name
  plate _ (Int a n) = pure $ Int a n
  plate f (Negate a ws expr) = Negate a ws <$> f expr

data BinOp a
  = Is a
  | Minus a
  | Exp a
  | BoolAnd a
  | BoolOr a
  | Multiply a
  | Divide a
  | Plus a
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
  [ entry BoolOr 4 L
  , entry BoolAnd 5 L
  , entry Is 10 L
  , entry Minus 20 L
  , entry Plus 20 L
  , entry Multiply 25 L
  , entry Divide 25 L
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

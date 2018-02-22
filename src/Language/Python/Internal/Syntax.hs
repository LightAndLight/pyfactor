{-# language DataKinds, PolyKinds #-}
module Language.Python.Internal.Syntax where

import Control.Lens.Plated
import Data.Functor

type Params v a = [Param v a]
data Param (v :: [*]) a
  = PositionalParam a String
  | KeywordParam a String (Expr v a)
  deriving (Eq, Show)

data Args (v :: [*]) a
  = NoArgs a
  | PositionalArg a (Expr v a) (Args v a)
  deriving (Eq, Show)

data Statement (v :: [*]) a
  = Fundef a String (Params v a) [Statement v a]
  | Return a (Expr v a)
  | Expr a (Expr v a)
  | If a (Expr v a) [Statement v a]
  | Assign a (Expr v a) (Expr v a)
  deriving (Eq, Show)
instance Plated (Statement v a) where
  plate f p@(Fundef _ _ _ sts) = traverse f sts $> p
  plate f p@(If _ _ sts) = traverse f sts $> p
  plate _ p = pure p

data Expr (v :: [*]) a
  = List a [Expr v a]
  | Deref a (Expr v a) String
  | Call a (Expr v a) (Args v a)
  | None a
  | Comp a (CompOp a) (Expr v a) (Expr v a)
  | Ident a String
  deriving (Eq, Show)

data CompOp a
  = Is a
  deriving (Eq, Show)

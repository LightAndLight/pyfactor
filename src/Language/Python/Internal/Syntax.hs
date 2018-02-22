{-# language DataKinds, PolyKinds #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances,
  MultiParamTypeClasses #-}
module Language.Python.Internal.Syntax where

import Control.Lens.TH
import Control.Lens.Plated
import Control.Lens.Wrapped

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

type Params v a = [Param v a]
data Param (v :: [*]) a
  = PositionalParam a String
  | KeywordParam a String (Expr v a)
  deriving (Eq, Show)

data Args (v :: [*]) a
  = NoArgs a
  | PositionalArg a (Expr v a) (Args v a)
  deriving (Eq, Show)

data Whitespace = Space | Tab | Continued [Whitespace] deriving (Eq, Show)

data Statement (v :: [*]) a
  = Fundef a String (Params v a) (Block v a)
  | Return a (Expr v a)
  | Expr a (Expr v a)
  | If a (Expr v a) (Block v a)
  | Assign a (Expr v a) (Expr v a)
  | Pass a
  deriving (Eq, Show)

data Block v a
  = Block
  { _blockAnn :: a
  , _blockWhitespace :: [Whitespace]
  , _blockStatements :: [Statement v a]
  }
  deriving (Eq, Show)
makeLenses ''Block

instance Plated (Statement v a) where
  plate f (Fundef a b c sts) = Fundef a b c <$> (blockStatements.traverse) f sts
  plate f (If a b sts) = If a b <$> (blockStatements.traverse) f sts
  plate _ p = pure p

{-# language DataKinds, PolyKinds #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances,
  MultiParamTypeClasses #-}
module Language.Python.Internal.Syntax where

import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Plated
import Control.Lens.Wrapped
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

data BinOp a
  = Is a
  deriving (Eq, Show)

makeWrapped ''Block

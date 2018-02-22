{-# language DataKinds, KindSignatures #-}
module Language.Python.Validate.Syntax.Error where

import Language.Python.Internal.Syntax

data SyntaxError (v :: [*]) a
  = PositionalAfterKeyword a String
  | CannotAssignTo a (Expr v a)
  | DuplicateArgument a String
  deriving (Eq, Show)

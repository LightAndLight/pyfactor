{-# language DataKinds, KindSignatures #-}
{-# language MultiParamTypeClasses, TemplateHaskell, FunctionalDependencies,
    FlexibleInstances #-}
module Language.Python.Validate.Syntax.Error where

import Control.Lens.TH
import Language.Python.Internal.Syntax

data SyntaxError (v :: [*]) a
  = PositionalAfterKeyword a String
  | CannotAssignTo a (Expr v a)
  | DuplicateArgument a String
  | MissingSpacesInExpr (Expr v a)
  deriving (Eq, Show)

makeClassyPrisms ''SyntaxError

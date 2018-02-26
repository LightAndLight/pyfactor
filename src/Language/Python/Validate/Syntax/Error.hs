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
  | MissingSpacesIn String String (Expr v a)
  | ExpectedNewlineAfter (a, [Whitespace], Statement v a, Maybe Newline)
  deriving (Eq, Show)

makeClassyPrisms ''SyntaxError

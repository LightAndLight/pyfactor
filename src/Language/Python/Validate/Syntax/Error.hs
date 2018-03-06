{-# language DataKinds, KindSignatures #-}
{-# language MultiParamTypeClasses, TemplateHaskell, FunctionalDependencies,
    FlexibleInstances #-}
module Language.Python.Validate.Syntax.Error where

import Control.Lens.TH
import Language.Python.Internal.Syntax

data SyntaxError (v :: [*]) a
  = PositionalAfterKeywordArg a (Expr v a)
  | PositionalAfterKeywordParam a String
  | CannotAssignTo a (Expr v a)
  | DuplicateArgument a String
  | MissingSpacesIn a String String
  | ExpectedNewlineAfter (a, [Whitespace], Statement v a, Maybe Newline)
  | IdentifierReservedWord a String
  deriving (Eq, Show)

makeClassyPrisms ''SyntaxError

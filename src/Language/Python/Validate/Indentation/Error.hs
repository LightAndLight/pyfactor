{-# language TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, DataKinds, KindSignatures #-}
module Language.Python.Validate.Indentation.Error where

import Language.Python.Internal.Syntax

import Control.Lens.TH

data IndentationError (v :: [*]) a = BadIndent [Whitespace] [Whitespace] a
  deriving (Eq, Show)

makeClassyPrisms ''IndentationError

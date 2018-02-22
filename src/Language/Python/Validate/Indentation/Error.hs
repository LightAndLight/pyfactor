{-# language TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, DataKinds, KindSignatures #-}
module Language.Python.Validate.Indentation.Error where

import Control.Lens.TH

data IndentationError (v :: [*]) a
  = ExpectedIndent a
  deriving (Eq, Show)

makeClassyPrisms ''IndentationError

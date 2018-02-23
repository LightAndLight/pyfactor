{-# language DataKinds #-}
module Main where

import Control.Lens
import Data.Foldable

import Example
import Data.Validate
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Internal.Render

main = do
  let x = append_to ()
  case validateStatementIndentation x of
    Failure errs -> print (errs :: [IndentationError '[] ()])
    Success a ->
      case validateStatementSyntax a of
        Failure errs -> print (errs :: [SyntaxError '[Indentation] ()])
        Success a' -> putStrLn . unlines $ renderStatement a'

  let x = rewrite fixMDA append_to'
  case validateStatementIndentation x of
    Failure errs -> print (errs :: [IndentationError '[] ()])
    Success a ->
      case validateStatementSyntax a of
        Failure errs -> print (errs :: [SyntaxError '[Indentation] ()])
        Success a' -> putStrLn . unlines $ renderStatement a'

  let x = append_to'' ()
  case validateStatementIndentation x of
    Failure errs -> print (errs :: [IndentationError '[] ()])
    Success a ->
      case validateStatementSyntax a of
        Failure errs -> print (errs :: [SyntaxError '[Indentation] ()])
        Success a' -> putStrLn . unlines $ renderStatement a'

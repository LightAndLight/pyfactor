{-# language DataKinds, TypeOperators, FlexibleContexts #-}
module Main where

import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error
import Generators

import Control.Lens
import Control.Monad.IO.Class
import Data.List
import Data.Validate
import System.Exit
import System.Process

import Hedgehog
import qualified Hedgehog.Gen as Gen

validateExprSyntax'
  :: Expr '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Expr '[Syntax, Indentation] a)
validateExprSyntax' = validateExprSyntax

validateExprIndentation'
  :: Expr '[] a
  -> Validate [IndentationError '[] a] (Expr '[Indentation] a)
validateExprIndentation' = validateExprIndentation

validateStatementSyntax'
  :: Statement '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Statement '[Syntax, Indentation] a)
validateStatementSyntax' = validateStatementSyntax

validateStatementIndentation'
  :: Statement '[] a
  -> Validate [IndentationError '[] a] (Statement '[Indentation] a)
validateStatementIndentation' = validateStatementIndentation

runPython3 :: (MonadTest m, MonadIO m) => String -> m ()
runPython3 str = do
  (ec, sto, ste) <- liftIO $ readProcessWithExitCode "python3" [] str
  case ec of
    ExitSuccess -> success
    ExitFailure{}
      | "SyntaxError" `isInfixOf` last (lines ste) -> do
          annotate ste
          failure
      | otherwise -> success

syntax_expr :: Property
syntax_expr =
  property $ do
    ex <-
      forAll $
      Gen.filter
        (has (_Success._Success) . fmap validateExprSyntax' . validateExprIndentation')
        genExpr
    let rex = renderExpr ex
    annotate rex
    runPython3 rex

syntax_statement :: Property
syntax_statement =
  property $ do
    st <-
      forAll $
      Gen.filter
        (has (_Success._Success) . fmap validateStatementSyntax' . validateStatementIndentation')
        genStatement
    let rst = renderLines $ renderStatement st
    annotate rst
    runPython3 rst

main = do
  check syntax_expr
  check syntax_statement

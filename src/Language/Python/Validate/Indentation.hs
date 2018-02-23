{-# language DataKinds #-}
{-# language TypeOperators #-}
module Language.Python.Validate.Indentation where

import Control.Applicative
import Control.Lens ((#), _Wrapped, view, from)
import Data.Coerce
import Data.Type.Set
import Data.Validate

import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation.Error

data Indentation

validateBlockIndentation
  :: AsIndentationError e v a
  => Block v a
  -> Validate [e] (Block (Nub (Indentation ': v)) a)
validateBlockIndentation (Block ann ws sts)
  | null ws = Failure [_ExpectedIndent # ann] <*> traverse validateStatementIndentation sts
  | otherwise = Block ann ws <$> traverse validateStatementIndentation sts

validateExprIndentation
  :: AsIndentationError e v a
  => Expr v a
  -> Validate [e] (Expr (Nub (Indentation ': v)) a)
validateExprIndentation e = pure $ coerce e

validateParamsIndentation
  :: AsIndentationError e v a
  => Params v a
  -> Validate [e] (Params (Nub (Indentation ': v)) a)
validateParamsIndentation e = pure $ coerce e

validateArgsIndentation
  :: AsIndentationError e v a
  => Args v a
  -> Validate [e] (Args (Nub (Indentation ': v)) a)
validateArgsIndentation e = pure $ coerce e

validateStatementIndentation
  :: AsIndentationError e v a
  => Statement v a
  -> Validate [e] (Statement (Nub (Indentation ': v)) a)
validateStatementIndentation (Fundef a name params body) =
  Fundef a name <$>
  validateParamsIndentation params <*>
  validateBlockIndentation body
validateStatementIndentation (If a expr body) =
  If a <$>
  validateExprIndentation expr <*>
  validateBlockIndentation body
validateStatementIndentation p = pure $ coerce p

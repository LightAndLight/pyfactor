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
validateBlockIndentation a =
  view (from _Wrapped) <$>
  go Nothing (view _Wrapped a)
  where
    go _ [] = pure []
    go a ((ann, ws, st):xs)
      | null ws = Failure [_ExpectedIndent # ann] <*> (go a xs)
      | otherwise =
          case a of
            Nothing ->
              liftA2 (:)
                ((,,) ann ws <$> validateStatementIndentation st)
              (go (Just ws) xs)
            Just ws'
              | ws == ws' ->
                  liftA2 (:)
                    ((,,) ann ws <$> validateStatementIndentation st)
                    (go a xs)
              | otherwise -> Failure [_WrongIndent # (ws', ws, ann)] <*> (go a xs)

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

{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
module Language.Python.Validate.Syntax where

import Control.Applicative
import Control.Lens ((#), _Wrapped)
import Control.Lens.Tuple
import Control.Lens.Traversal
import Data.Coerce
import Data.Semigroup
import Data.Type.Set
import Data.Validate
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax.Error

data Syntax

validateExprSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Expr v a
  -> Validate [e] (Expr (Nub (Syntax ': v)) a)
validateExprSyntax (Parens a e) = Parens a <$> validateExprSyntax e
validateExprSyntax (Bool a b) = pure $ Bool a b
validateExprSyntax (String a b) = pure $ String a b
validateExprSyntax (Negate a expr) = Negate a <$> validateExprSyntax expr
validateExprSyntax (Int a n) = pure $ Int a n
validateExprSyntax (Ident a name) = pure $ Ident a name
validateExprSyntax (List a exprs) = List a <$> traverse validateExprSyntax exprs
validateExprSyntax (Deref a expr name) =
  Deref a <$>
  validateExprSyntax expr <*>
  pure name
validateExprSyntax (Call a expr args) =
  Call a <$>
  validateExprSyntax expr <*>
  validateArgsSyntax args
validateExprSyntax (None a) = pure $ None a
validateExprSyntax (BinOp a op e1 e2) =
  BinOp a op <$>
  validateExprSyntax e1 <*>
  validateExprSyntax e2

validateStatementSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Statement v a
  -> Validate [e] (Statement (Nub (Syntax ': v)) a)
validateStatementSyntax (Fundef a name params body) =
  Fundef a name <$>
  validateParamsSyntax params <*>
  traverseOf (_Wrapped.traverse._3) validateStatementSyntax body
validateStatementSyntax (Return a expr) =
  Return a <$>
  validateExprSyntax expr
validateStatementSyntax (Expr a expr) =
  Expr a <$>
  validateExprSyntax expr
validateStatementSyntax (If a expr body body') =
  If a <$>
  validateExprSyntax expr <*>
  traverseOf (_Wrapped.traverse._3) validateStatementSyntax body <*>
  traverseOf (traverse._Wrapped.traverse._3) validateStatementSyntax body'
validateStatementSyntax (While a expr body) =
  While a <$>
  validateExprSyntax expr <*>
  traverseOf (_Wrapped.traverse._3) validateStatementSyntax body
validateStatementSyntax (Assign a lvalue rvalue) =
  Assign a <$>
  (if canAssignTo lvalue
   then validateExprSyntax lvalue
   else Failure [_CannotAssignTo # (a, lvalue)]) <*>
  validateExprSyntax rvalue
validateStatementSyntax p@Pass{} = pure $ coerce p
validateStatementSyntax p@Break{} = pure $ coerce p

canAssignTo :: Expr v a -> Bool
canAssignTo None{} = False
canAssignTo _ = True

validateArgsSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Args v a -> Validate [e] (Args (Nub (Syntax ': v)) a)
validateArgsSyntax (NoArgs a) = pure $ NoArgs a
validateArgsSyntax (PositionalArg a expr args) =
  PositionalArg a <$>
  validateExprSyntax expr <*>
  validateArgsSyntax args

validateParamsSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Params v a -> Validate [e] (Params (Nub (Syntax ': v)) a)
validateParamsSyntax = go [] False
  where
    go _ _ [] = pure []
    go names False (PositionalParam a name : params)
      | name `elem` names = 
          Failure [_DuplicateArgument # (a, name)] <*> go (name:names) False params
      | otherwise = 
          (PositionalParam a name :) <$> go (name:names) False params
    go names True (PositionalParam a name : params) =
      let errs =
            [_DuplicateArgument # (a, name) | name `elem` names] <>
            [_PositionalAfterKeyword # (a, name)]
      in
        Failure errs <*> go (name:names) True params
    go names _ (KeywordParam a name expr : params) =
      liftA2 (:)
        (KeywordParam a name <$> validateExprSyntax expr)
        (go (name:names) True params)

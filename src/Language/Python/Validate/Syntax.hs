{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
module Language.Python.Validate.Syntax where

import Control.Applicative
import Data.Semigroup
import Data.Type.Set
import Data.Validate
import Language.Python.Internal.Syntax
import Language.Python.Validate.Syntax.Error

data Syntax

validateExpr :: Expr v a -> Validate [SyntaxError v a] (Expr (Nub (Syntax ': v)) a)
validateExpr (Ident a name) = pure $ Ident a name
validateExpr (List a exprs) = List a <$> traverse validateExpr exprs
validateExpr (Deref a expr name) =
  Deref a <$>
  validateExpr expr <*>
  pure name
validateExpr (Call a expr args) =
  Call a <$>
  validateExpr expr <*>
  validateArgs args
validateExpr (None a) = pure $ None a
validateExpr (Comp a op e1 e2) =
  Comp a op <$>
  validateExpr e1 <*>
  validateExpr e2

validateStatement
  :: Statement v a
  -> Validate [SyntaxError v a] (Statement (Nub (Syntax ': v)) a)
validateStatement (Fundef a name params body) =
  Fundef a name <$>
  validateParams params <*>
  traverse validateStatement body
validateStatement (Return a expr) =
  Return a <$>
  validateExpr expr
validateStatement (Expr a expr) =
  Expr a <$>
  validateExpr expr
validateStatement (If a expr body) =
  If a <$>
  validateExpr expr <*>
  traverse validateStatement body
validateStatement (Assign a lvalue rvalue) =
  Assign a <$>
  (if canAssignTo lvalue
   then validateExpr lvalue
   else Failure [CannotAssignTo a lvalue]) <*>
  validateExpr rvalue

canAssignTo :: Expr v a -> Bool
canAssignTo None{} = False
canAssignTo _ = True

validateArgs :: Args v a -> Validate [SyntaxError v a] (Args (Nub (Syntax ': v)) a)
validateArgs (NoArgs a) = pure $ NoArgs a
validateArgs (PositionalArg a expr args) =
  PositionalArg a <$>
  validateExpr expr <*>
  validateArgs args

validateParams :: Params v a -> Validate [SyntaxError v a] (Params (Nub (Syntax ': v)) a)
validateParams = go [] False
  where
    go _ _ [] = pure []
    go names False (PositionalParam a name : params)
      | name `elem` names = 
          Failure [DuplicateArgument a name] <*> go (name:names) False params
      | otherwise = 
          (PositionalParam a name :) <$> go (name:names) False params
    go names True (PositionalParam a name : params) =
      let errs =
            [DuplicateArgument a name | name `elem` names] <>
            [PositionalAfterKeyword a name]
      in
        Failure errs <*> go (name:names) True params
    go names _ (KeywordParam a name expr : params) =
      liftA2 (:)
        (KeywordParam a name <$> validateExpr expr)
        (go (name:names) True params)

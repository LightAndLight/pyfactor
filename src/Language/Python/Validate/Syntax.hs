{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
module Language.Python.Validate.Syntax where

import Control.Applicative
import Control.Lens ((#))
import Data.Semigroup
import Data.Type.Set
import Data.Validate
import Language.Python.Internal.Syntax
import Language.Python.Validate.Syntax.Error

data Syntax

validateExpr :: AsSyntaxError e v a => Expr v a -> Validate [e] (Expr (Nub (Syntax ': v)) a)
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
  :: AsSyntaxError e v a
  => Statement v a
  -> Validate [e] (Statement (Nub (Syntax ': v)) a)
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
   else Failure [_CannotAssignTo # (a, lvalue)]) <*>
  validateExpr rvalue

canAssignTo :: Expr v a -> Bool
canAssignTo None{} = False
canAssignTo _ = True

validateArgs
  :: AsSyntaxError e v a
  => Args v a -> Validate [e] (Args (Nub (Syntax ': v)) a)
validateArgs (NoArgs a) = pure $ NoArgs a
validateArgs (PositionalArg a expr args) =
  PositionalArg a <$>
  validateExpr expr <*>
  validateArgs args

validateParams
  :: AsSyntaxError e v a
  => Params v a -> Validate [e] (Params (Nub (Syntax ': v)) a)
validateParams = go [] False
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
        (KeywordParam a name <$> validateExpr expr)
        (go (name:names) True params)

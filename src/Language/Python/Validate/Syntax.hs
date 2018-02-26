{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
module Language.Python.Validate.Syntax where

import Control.Applicative
import Control.Lens ((#), _Wrapped)
import Control.Lens.Tuple
import Control.Lens.Traversal
import Data.Char
import Data.Coerce
import Data.Semigroup
import Data.Type.Set
import Data.Validate
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax.Error

data Syntax

isIdentifierChar :: Char -> Bool
isIdentifierChar = liftA2 (||) isLetter (=='_')

binOpStartsWith :: BinOp a -> Char
binOpStartsWith (Is a) = 'i'
binOpStartsWith (Minus a) = '-'
binOpStartsWith (Exp a) = '*'
binOpStartsWith (BoolAnd a) = 'a'
binOpStartsWith (BoolOr a) = 'o'
binOpStartsWith (Multiply a) = '*'
binOpStartsWith (Divide a) = '/'
binOpStartsWith (Plus a) = '+'

binOpEndsWith :: BinOp a -> Char
binOpEndsWith (Is a) = 's'
binOpEndsWith (Minus a) = '-'
binOpEndsWith (Exp a) = '*'
binOpEndsWith (BoolAnd a) = 'd'
binOpEndsWith (BoolOr a) = 'r'
binOpEndsWith (Multiply a) = '*'
binOpEndsWith (Divide a) = '/'
binOpEndsWith (Plus a) = '+'

exprStartsWith :: Expr v a -> Char
exprStartsWith (List _ _) = '['
exprStartsWith (Deref _ e _) = exprStartsWith e
exprStartsWith (Call _ e _) = exprStartsWith e
exprStartsWith None{} = 'N'
exprStartsWith (BinOp _ _ _ _ e _) = exprStartsWith e
exprStartsWith (Negate _ _) = '-'
exprStartsWith Parens{} = '('
exprStartsWith (Ident _ s) = head s
exprStartsWith (Int _ i) = head $ show i
exprStartsWith (Bool _ b) = head $ show b

exprEndsWith :: Expr v a -> Char
exprEndsWith (List _ _) = ']'
exprEndsWith (Deref a _ s) =
  case s of
    [] -> '.'
    _ -> last s
exprEndsWith Call{} = ')'
exprEndsWith None{} = 'e'
exprEndsWith (BinOp _ _ _ _ _ e) = exprEndsWith e
exprEndsWith (Negate _ e) = exprEndsWith e
exprEndsWith Parens{} = ')'
exprEndsWith (Ident _ s) = last s
exprEndsWith (Int _ i) = last $ show i
exprEndsWith (Bool _ b) = last $ show b

validateExprSyntax
  :: ( AsSyntaxError e v a
     , Member Indentation v
     )
  => Expr v a
  -> Validate [e] (Expr (Nub (Syntax ': v)) a)
validateExprSyntax (Parens a e) = Parens a <$> validateExprSyntax e
validateExprSyntax (Bool a b) = pure $ Bool a b
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
validateExprSyntax e@(BinOp a op ws1 ws2 e1 e2) =
  uncurry (BinOp a op) <$>
  (if
     (null ws1 &&
      isIdentifierChar (exprEndsWith e1) &&
      isIdentifierChar (binOpStartsWith op)) ||
     (null ws2 &&
      isIdentifierChar (binOpEndsWith op) &&
      isIdentifierChar (exprStartsWith e2))
   then
     Failure [_MissingSpacesInExpr # e]
   else
     Success (ws1, ws2)) <*>
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
validateStatementSyntax (If a expr body) =
  If a <$>
  validateExprSyntax expr <*>
  traverseOf (_Wrapped.traverse._3) validateStatementSyntax body
validateStatementSyntax (Assign a lvalue rvalue) =
  Assign a <$>
  (if canAssignTo lvalue
   then validateExprSyntax lvalue
   else Failure [_CannotAssignTo # (a, lvalue)]) <*>
  validateExprSyntax rvalue
validateStatementSyntax p@Pass{} = pure $ coerce p

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

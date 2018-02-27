{-# language DataKinds #-}
{-# language OverloadedStrings, OverloadedLists #-}
{-# language FlexibleContexts #-}
module Example where

import Control.Lens
  ((^?), (^?!), (&), (.~), (%~), (^.), _last, _init)
import Control.Lens.Fold
import Control.Lens.Plated
import Control.Lens.Prism
import Control.Lens.Tuple
import Data.Semigroup
import GHC.Natural
import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax
import Language.Python.Syntax

{-
def append_to(element, to=[]):
    to.append(element)
    return to
-}
append_to a =
  Fundef a
    [Space]
    "append_to"
    []
    [ PositionalParam a "element"
    , KeywordParam a "to" (List a [] CommaSepNone [])
    ]
    []
    []
    LF
    (Block
     [ (a, replicate 4 Space, Expr a $ Call a (Deref a (Ident a "to") [] [] "append") [] (PositionalArg a (Ident a "element") $ NoArgs a), Just LF)
     , (a, replicate 4 Space, Return a [Space] (Ident a "to"), Just LF)
     ])

{-
def append_to(element, to=[]):
    to.append(element)
    return to
-}

isMutable :: Expr v a -> Bool
isMutable None{} = False
isMutable List{} = True
isMutable Deref{} = True
isMutable Call{} = True
isMutable BinOp{} = True
isMutable Negate{} = True
isMutable (Parens _ _ a _) = isMutable a
isMutable Ident{} = True
isMutable Int{} = False
isMutable Bool{} = False
isMutable String{} = False


append_to' =
  let
    d = def_ "append_to" [ p_ "element", k_ "to" (list_ []) ]
  in
    d
      [ d [pass_]
      , expr_ $ call_ ("to" /> "append") [ "element" ]
      , return_ "to"
      ]

append_to'' a =
  Fundef a
    [Space]
    "append_to"
    []
    [ PositionalParam a "element"
    , KeywordParam a "to" (List a [] CommaSepNone [])
    ]
    []
    []
    LF
    (Block
     [ (a, replicate 4 Space, Expr a $ Call a (Deref a (Ident a "to") [] [] "append") [] (PositionalArg a (Ident a "element") $ NoArgs a), Just LF)
     , (a, replicate 4 Space ++ [Continued [Space, Space]], Return a [Space] (Ident a "to"), Just LF)
     ])

bracketing =
  def_ "bracketing" []
    [ expr_ $ 1 .- 2 .- 3
    , expr_ $ 1 .- (2 .- 3)
    , expr_ $ 1 .- 2 `is_` 3
    , expr_ $ 1 .- (2 `is_` 3)
    , expr_ $ 1 .** 2 .** 3
    , expr_ $ (1 .** 2) .** 3
    , expr_ $ -1 .** 2
    , expr_ $ (-1) .** 2
    , expr_ $ -1 .- (-1)
    , expr_ true_
    , expr_ false_
    , expr_ $ or_ (false_ `and_` false_) true_
    ]

-- | Fix mutable default arguments
fixMDA :: Statement '[] () -> Maybe (Statement '[] ())
fixMDA input = do
  (_, _, name, _, params, _, _, _, body) <- input ^? _Fundef
  targetParam <- params ^? folded._KeywordParam.filtered (isMutable._kpExpr)

  let
    pname = targetParam ^. kpName

    newparams =
      params & traverse._KeywordParam.filtered (isMutable._kpExpr).kpExpr .~ none_

    fixed =
      if_ (var_ pname `is_` none_) [ var_ pname .= list_ [] ]

  pure $
    def_ name newparams (fixed : (body ^.. _Statements))

indentSpaces :: Natural -> Statement v a -> Statement v a
indentSpaces n = transform (_Indents .~ replicate (fromIntegral n) Space)

indentTabs :: Statement v a -> Statement v a
indentTabs = transform (_Indents .~ [Tab])

fact_tr =
  def_ "fact" [p_ "n"]
  [ def_ "go" [p_ "n", p_ "acc"]
    [ ifElse_ ("n" .== 0)
      [return_ "acc"]
      [return_ $ call_ "go" [p_ $ "n" .- 1, p_ $ "n" .* "acc"]]
    ]
  , return_ $ call_ "go" [p_ "n", p_ 1]
  ]

spin = def_ "spin" [] [expr_ $ call_ "spin" []]

yes =
  def_ "yes" []
  [ expr_ $ call_ "print" [p_ $ str_ "yes"]
  , expr_ $ call_ "yes" []
  ]

optimize_tr st = do
  (_, _, name, _, params, _, _, _, body) <- st ^? _Fundef
  bodyLast <- toListOf (unvalidated._Statements) body ^? _last
  let paramNames = _paramName <$> params
  if not $ hasTC name bodyLast
    then Nothing
    else
      Just .
      def_ name params $
        zipWith (\a b -> var_ (a <> "__tr") .= var_ b) paramNames paramNames <>
        [ "__res__tr" .= none_
        , while_ true_ .
          transformOn (traverse._Exprs) (renameIn paramNames "__tr") $
          (toListOf (unvalidated._Statements) body ^?! _init) <>
          looped name paramNames bodyLast
        , return_ "__res__tr"
        ]
  where
    isTailCall :: String -> Expr '[] () -> Bool
    isTailCall name e
      | anyOf (cosmos._Call._2._Ident._2) (== name) e
      = (e ^? _Call._2._Ident._2) == Just name
      | otherwise = False

    hasTC :: String -> Statement '[] () -> Bool
    hasTC name st =
      case st of
        Return _ _ e -> isTailCall name e
        Expr _ e -> isTailCall name e
        If _ e sts sts' ->
          allOf _last (hasTC name) (sts ^.. _Statements) ||
          allOf _last (hasTC name) (sts' ^.. _Just._Statements)
        _ -> False

    renameIn :: [String] -> String -> Expr '[] () -> Expr '[] ()
    renameIn params suffix =
      transform
        (_Ident._2 %~ (\a -> if a `elem` params then a <> suffix else a))

    looped :: String -> [String] -> Statement '[] () -> [Statement '[] ()]
    looped name params r@(Return _ _ e) =
      case e ^? _Call of
        Just (_, f, _, args)
          | Just name' <- f ^? _Ident._2
          , name' == name ->
              fmap (\a -> var_ (a <> "__tr__old") .= (var_ $ a <> "__tr")) params <>
              zipWith
                (\a b -> var_ (a <> "__tr") .= b)
                params
                (transformOn traverse (renameIn params "__tr__old") $ args ^.. _Exprs)
        _ -> [ "__res__tr" .= e, break_ ]
    looped name params r@(Expr _ e)
      | isTailCall name e = [pass_]
      | otherwise = [r]
    looped name params r@(If _ e sts sts')
      | hasTC name r =
          case sts' of
            Nothing ->
              [ if_ e
                  ((toListOf _Statements sts ^?! _init) <>
                   looped name params (toListOf _Statements sts ^?! _last))
              ]
            Just sts'' ->
              [ ifElse_ e
                  ((toListOf _Statements sts ^?! _init) <>
                   looped name params (toListOf _Statements sts ^?! _last))
                  ((toListOf _Statements sts'' ^?! _init) <>
                   looped name params (toListOf _Statements sts'' ^?! _last))
              ]
      | otherwise = [r]
    looped name params s = [s]

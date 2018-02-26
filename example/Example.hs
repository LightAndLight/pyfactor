{-# language DataKinds #-}
{-# language OverloadedStrings, OverloadedLists #-}
module Example where

import Control.Lens
  ((^?), (&), (.~), (^.), (^..), folded, filtered, transform)
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
     , (a, replicate 4 Space, Return a (Ident a "to"), Just LF)
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
     , (a, replicate 4 Space ++ [Continued [Space, Space]], Return a (Ident a "to"), Just LF)
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

{-
def append_to(element, to=None):
    if to is None:
        to = []
    to.append(element)
    return to
-}

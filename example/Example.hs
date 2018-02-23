{-# language OverloadedStrings #-}
module Example where

import Control.Lens
  ((^?), (&), (.~), (^.), (^..), folded, filtered)
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
    "append_to"
    [ PositionalParam a "element"
    , KeywordParam a "to" (List a [])
    ]
    (Block
     [ (a, replicate 4 Space, Expr a $ Call a (Deref a (Ident a "to") "append") (PositionalArg a (Ident a "element") $ NoArgs a))
     , (a, replicate 4 Space, Return a (Ident a "to"))
     ])

{-
def append_to(element, to=[]):
    to.append(element)
    return to
-}

isMutable :: Expr v a -> Bool
isMutable None{} = False
isMutable _ = True

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
    "append_to"
    [ PositionalParam a "element"
    , KeywordParam a "to" (List a [])
    ]
    (Block
     [ (a, replicate 4 Space, Expr a $ Call a (Deref a (Ident a "to") "append") (PositionalArg a (Ident a "element") $ NoArgs a))
     , (a, replicate 4 Space ++ [Continued [Space, Space]], Return a (Ident a "to"))
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
    ]

-- | Fix mutable default arguments
fixMDA input = do
  (_, name, params, body) <- input ^? _Fundef
  targetParam <- params ^? folded._KeywordParam.filtered (isMutable._kpExpr)

  let
    pname = targetParam ^. kpName

    newparams =
      params & traverse._KeywordParam.filtered (isMutable._kpExpr).kpExpr .~ none_

    fixed =
      if_ (var_ pname `is_` none_) [ var_ pname .= list_ [] ]

  pure $
    def_ name newparams (fixed : (body ^.. _Statements))

{-
def append_to(element, to=None):
    if to is None:
        to = []
    to.append(element)
    return to
-}

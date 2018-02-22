{-# language OverloadedStrings #-}
module Example where

import Control.Lens ((^?), (&), (.~), (^..), folded, _2, _3, view, filtered)
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
    [ (a, replicate 4 Space, Expr a $ Call a (Deref a (Ident a "to") "append") (PositionalArg a (Ident a "element") $ NoArgs a))
    , (a, replicate 4 Space, Return a (Ident a "to"))
    ]


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
      , expr_ $ call_ ("to" .> "append") [ "element" ]
      , return_ "to"
      ]

append_to'' =
  def_ "append_to" [ p_ "element", k_ "to" (list_ []), k_ "to2" (list_ []) ]
    [ expr_ $ call_ ("to" .> "append") [ "element" ]
    , return_ "to"
    ]

fixMDA input = do
  (_, name, params, body) <- input ^? _Fundef
  (_, paramname, paramvalue) <- params ^? folded._KeywordParam.filtered (isMutable.view _3)
  let
    newparams =
        params & traverse._KeywordParam.filtered ((==paramname).view _2)._3 .~ none_

    fixed =
        if_ (var_ paramname `is_` none_) [ var_ paramname .= list_ [] ]

  pure $ def_ name newparams (fixed : (body ^.. folded._3))

{-
def append_to(element, to=None):
    if to is None:
        to = []
    to.append(element)
    return to
-}

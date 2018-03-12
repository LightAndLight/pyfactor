{-# language DataKinds #-}
module Generators where

import Control.Applicative
import Control.Monad
import Data.List.NonEmpty
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax

genNewline :: MonadGen m => m Newline
genNewline = Gen.element [CR, LF, CRLF]

genWhitespace :: MonadGen m => m Whitespace
genWhitespace =
  Gen.recursive Gen.choice
    [pure Space, pure Tab]
    [Continued <$> genNewline <*> genWhitespaces]

genWhitespaces :: MonadGen m => m [Whitespace]
genWhitespaces = Gen.list (Range.linear 0 5) genWhitespace

genWhitespaces1 :: MonadGen m => m (NonEmpty Whitespace)
genWhitespaces1 = Gen.nonEmpty (Range.linear 1 5) genWhitespace

genString :: MonadGen m => m String
genString = Gen.list (Range.linear 0 50) Gen.unicode

genIdent :: MonadGen m => m (Ident '[] ())
genIdent = MkIdent () <$> genString

genCommaSep :: MonadGen m => Range Int -> m a -> m (CommaSep a)
genCommaSep r m = do
  s <- Gen.integral_ r
  Gen.sized $ \n -> go (n `div` Size s) s
  where
    go s 0 = pure CommaSepNone
    go s 1 =
      Gen.choice
        [ CommaSepOne <$> Gen.resize s m
        , CommaSepMany <$>
          Gen.resize s m <*>
          genWhitespaces <*>
          genWhitespaces <*>
          go s 0
        ]
    go s n =
      CommaSepMany <$>
      Gen.resize s m <*>
      genWhitespaces <*>
      genWhitespaces <*>
      go s (n-1)

genArg :: MonadGen m => m (Arg '[] ())
genArg =
  Gen.recursive Gen.choice
    [ PositionalArg () <$> genExpr ]
    [ KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genExpr ]

genParam :: MonadGen m => m (Param '[] ())
genParam =
  Gen.choice
    [ PositionalParam () <$> genIdent
    , KeywordParam () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genExpr
    ]

genExpr :: MonadGen m => m (Expr '[] ())
genExpr =
  Gen.recursive Gen.choice
    [ genNone
    , Ident () <$> genIdent
    , genInt
    , genBool
    , String () <$> genString
    ]
    [ genList
    , genDeref
    , genCall
    , genBinOp
    , genNegate
    , genParens
    ]
  where
    genList =
      List () <$>
      genWhitespaces <*>
      genCommaSep (Range.linear 0 5) genExpr <*>
      genWhitespaces
    genDeref =
      Deref () <$>
      genExpr <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genIdent
    genCall =
      Call () <$>
      genExpr <*>
      genWhitespaces <*>
      genCommaSep (Range.linear 0 5) genArg
    genNone = pure $ None ()
    genBool = Bool () <$> Gen.bool
    genBinOp =
      BinOp () <$>
      genExpr <*>
      genWhitespaces <*>
      genOp <*>
      genWhitespaces <*>
      genExpr
    genOp = Gen.element $ _opOperator <$> operatorTable
    genNegate = Negate () <$> genWhitespaces <*> genExpr
    genParens = Parens () <$> genWhitespaces <*> genExpr <*> genWhitespaces
    genInt = Int () <$> Gen.integral (Range.constant (-2^16) (2^16))

genBlock :: MonadGen m => Range Int -> m (Block '[] ())
genBlock r = do
  n <- Size <$> Gen.integral_ r
  when (n == 0) $ error "cannot generate block of size 0"
  Block . NonEmpty.fromList <$> Gen.sized (\s -> go (s `div` n) n)
  where
    go _ 0 = pure []
    go s n =
      liftA2
        (:)
        (Gen.resize s $
         (,,,) () <$>
         genWhitespaces <*>
         genStatement <*>
         Gen.maybe genNewline)
        (go s $ n-1)

genStatement :: MonadGen m => m (Statement '[] ())
genStatement =
  Gen.recursive Gen.choice
    [ genPass
    , genBreak
    , genAssign
    , genEx
    , genReturn
    ]
    [ genFundef
    , genIf
    , genWhile
    ]
  where
    genFundef =
      Fundef () <$>
      genWhitespaces1 <*>
      genIdent <*>
      genWhitespaces <*>
      genCommaSep (Range.linear 0 5) genParam <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genNewline <*>
      genBlock (Range.exponential 1 5)
    genReturn = Return () <$> genWhitespaces <*> genExpr
    genEx = Expr () <$> genExpr
    genIf =
      If () <$>
      genWhitespaces <*>
      genExpr <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genNewline <*>
      genBlock (Range.exponential 1 5) <*>
      Gen.maybe
        ((,,,) <$>
         genWhitespaces <*>
         genWhitespaces <*>
         genNewline <*>
         genBlock (Range.exponential 1 5))
    genWhile =
      While () <$>
      genWhitespaces <*>
      genExpr <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genNewline <*>
      genBlock (Range.exponential 1 5)
    genPass = pure $ Pass ()
    genBreak = pure $ Break ()
    genAssign =
      Assign () <$>
      genExpr <*>
      genWhitespaces <*>
      genWhitespaces <*>
      genExpr

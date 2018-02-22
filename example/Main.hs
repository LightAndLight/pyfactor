module Main where

import Control.Lens
import Data.Foldable

import Example
import Data.Validate
import Language.Python.Validate.Syntax
import Language.Python.Internal.Render

main = do
  let x = append_to ()
  case validateStatement x of
    Failure errs -> print errs
    Success a -> putStrLn . unlines $ renderStatement a

  let x = rewrite fixMDA append_to'
  case validateStatement x of
    Failure errs -> print errs
    Success a -> putStrLn . unlines $ renderStatement a

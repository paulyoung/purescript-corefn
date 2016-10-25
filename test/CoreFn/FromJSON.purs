module Test.CoreFn.FromJSON
  ( testFromJSON
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import CoreFn.Comment (Comment(..))
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module(..))
import CoreFn.ModuleName (ModuleName(..))
import Data.Either (either)
import Data.Foreign (ForeignError(..))

-- TODO: use purescript-test-unit when compatible with 0.10
testFromJSON :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testFromJSON = do

  expectLeft (moduleFromJSON """
    {}
  """) \(x) ->
    assertEqual x (JSONError "Module name not found")

  expectRight (moduleFromJSON """
    {
      "Main": {
        "imports": [],
        "comments": []
      }
    }
  """) \(Module x) ->
    assertEqual x.moduleName (ModuleName "Main")

  expectRight (moduleFromJSON """
    {
      "Main": {
        "imports": [],
        "comments": [
          {
            "BlockComment": "A block comment"
          },
          {
            "LineComment": "A line comment"
          }
        ]
      }
    }
  """) \(Module x) ->
    assertEqual x.moduleComments
    [ (BlockComment "A block comment")
    , (LineComment "A line comment")
    ]

  expectRight (moduleFromJSON """
    {
      "Main": {
        "imports": [
          "Prim"
        ],
        "comments": []
      }
    }
  """) \(Module x) ->
    assertEqual x.moduleImports
      [ (ModuleName "Prim")
      ]

  where

  assertEqual :: forall a. (Eq a, Show a) => a -> a -> Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
  assertEqual actual expected =
    if actual == expected
      then logSuccessShow actual
      else fail (show expected) (show actual)

  fail expected actual = failure $ "\n"
    <> "  expected:\n"
    <> "    " <> expected <> "\n"
    <> "  actual:\n"
    <> "    " <> actual <> "\n"

  expectedLeft x = fail "Left" $ show x
  expectedRight x = fail "Right" $ show x

  expectLeft x f = either f expectedLeft x
  expectRight x g = either expectedRight g x

  green = "\x1b[32m"
  reset = "\x1b[0m"
  check = "✓"
  greenCheck = green <> check <> reset

  logSuccess x = log $ "  " <> greenCheck <> " " <> x

  logSuccessShow :: forall a. (Show a) => a -> Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
  logSuccessShow x = logSuccess $ show x

  failure = throwException <<< error

module Test.Util
  ( assertEqual
  , expectLeft
  , expectRight
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Data.Either (either, Either)

assertEqual :: forall a e. (Eq a, Show a) => a -> a -> Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
assertEqual actual expected =
  if actual == expected
    then logSuccessShow actual
    else fail (show expected) (show actual)

fail :: forall e. String -> String -> Eff (err :: EXCEPTION | e) Unit
fail expected actual = failure $ "\n"
  <> "  expected:\n"
  <> "    " <> expected <> "\n"
  <> "  actual:\n"
  <> "    " <> actual <> "\n"

expectedLeft ::  forall a e. (Show a) => a -> Eff (err :: EXCEPTION | e) Unit
expectedLeft x = fail "Left" $ show x

expectedRight ::  forall a e. (Show a) => a -> Eff (err :: EXCEPTION | e) Unit
expectedRight x = fail "Right" $ show x

expectLeft :: forall a b c
             . (Show c)
             => Either a c
             -> (a -> Eff (err :: EXCEPTION | b) Unit)
             -> Eff (err :: EXCEPTION | b) Unit
expectLeft x f = either f expectedLeft x

expectRight :: forall a b c
             . (Show c)
             => Either c a
             -> (a -> Eff (err :: EXCEPTION | b) Unit)
             -> Eff (err :: EXCEPTION | b) Unit
expectRight x g = either expectedRight g x

green :: String
green = "\x1b[32m"

reset :: String
reset = "\x1b[0m"

check :: String
check = "✓"

greenCheck :: String
greenCheck = green <> check <> reset

logSuccess :: forall e. String -> Eff (console :: CONSOLE | e) Unit
logSuccess x = log $ "  " <> greenCheck <> " " <> x

logSuccessShow :: forall a e. (Show a) => a -> Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
logSuccessShow x = logSuccess $ show x

failure :: forall e. String -> Eff (err :: EXCEPTION | e) Unit
failure = throwException <<< error

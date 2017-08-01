module Test.Util
  ( assertEqual
  , expectFailure
  , expectSuccess
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Control.Monad.Except (runExcept)
import Data.Either (either, Either)
import Data.Foreign (F, ForeignError)
import Data.List.Types (NonEmptyList)

assertEqual
  :: forall a eff
   . Eq a
  => Show a
  => a
  -> a
  -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
assertEqual actual expected =
  if actual == expected
    then logSuccessShow actual
    else fail (show expected) (show actual)

fail :: forall eff. String -> String -> Eff (exception :: EXCEPTION | eff) Unit
fail expected actual = failure $ "\n"
  <> "  expected:\n"
  <> "    " <> expected <> "\n"
  <> "  actual:\n"
  <> "    " <> actual <> "\n"

expectedLeft :: forall a eff. Show a => a -> Eff (exception :: EXCEPTION | eff) Unit
expectedLeft x = fail "Left" $ show x

expectedRight :: forall b eff. Show b => b -> Eff (exception :: EXCEPTION | eff) Unit
expectedRight x = fail "Right" $ show x

expectLeft
  :: forall a b eff
   . Show b
  => String
  -> Either a b
  -> (a -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit)
  -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
expectLeft description x f = do
  log $ "  " <> description
  either f expectedLeft x

expectRight
  :: forall a b eff
   . Show a
  => String
  -> Either a b
  -> (b -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit)
  -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
expectRight description x g = do
  log $ "  " <> description
  either expectedRight g x

expectFailure
  :: forall a eff
   . Show a
  => String
  -> F a
  -> (NonEmptyList ForeignError -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit)
  -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
expectFailure description x = expectLeft description (runExcept x)

expectSuccess
  :: forall a eff
   . String
  -> F a
  -> (a -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit)
  -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
expectSuccess description x = expectRight description (runExcept x)

green :: String
green = "\x1b[32m"

reset :: String
reset = "\x1b[0m"

check :: String
check = "✓"

greenCheck :: String
greenCheck = green <> check <> reset

logSuccess :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
logSuccess x = log $ "    " <> greenCheck <> " " <> x

logSuccessShow
  :: forall a eff
   . Show a
  => a
  -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) Unit
logSuccessShow x = logSuccess $ show x

failure :: forall eff. String -> Eff (exception :: EXCEPTION | eff) Unit
failure = throwException <<< error

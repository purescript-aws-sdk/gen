module Eff where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Except (Except, runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Foreign (MultipleErrors, renderForeignError)
import Data.Maybe (Maybe(..))

liftExcept :: forall eff a. Except MultipleErrors a -> Eff(exception :: EXCEPTION | eff) a
liftExcept except = case (runExcept except) of
    Right good -> pure good
    Left bads -> map renderForeignError bads
      # foldl (\text -> \line -> text <> "\n" <> line) ""
      # error
      # throwException

liftEither :: forall eff a. Either String a -> Eff(exception :: EXCEPTION | eff) a
liftEither (Right good) = pure good
liftEither (Left bad) = throwException $ error bad

liftMaybe :: forall eff a. Maybe a -> Eff(exception :: EXCEPTION | eff) a
liftMaybe (Just good) = pure good
liftMaybe Nothing = throwException $ error "No such element"

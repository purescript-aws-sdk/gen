module Eff where

import Prelude

import Effect (Effect)
import Effect.Exception (error, throwException)
import Control.Monad.Except (Except, runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Foreign (MultipleErrors, renderForeignError)


liftExcept :: forall a. Except MultipleErrors a -> Effect a
liftExcept except = case (runExcept except) of
    Right good -> pure good
    Left bads -> map renderForeignError bads
      # foldl (\text -> \line -> text <> "\n" <> line) ""
      # error
      # throwException

liftEither :: forall a. Either String a -> Effect a
liftEither (Right good) = pure good
liftEither (Left bad) = throwException $ error bad

liftMaybe :: forall a. Maybe a -> Effect a
liftMaybe (Just good) = pure good
liftMaybe Nothing = throwException $ error "No such element"

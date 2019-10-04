module Test.Main
       ( main
       ) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Printer.ServiceReaderSpec (serviceReaderSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  serviceReaderSpec

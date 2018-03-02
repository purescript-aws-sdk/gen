module Printer.PureScript.Comment where

import Prelude
import Data.String (Pattern(Pattern), Replacement(Replacement), replaceAll)

comment :: String -> String
comment str = commentPrefix <> commentedSrt
    where
        commentPrefix = "\n-- | "
        commentedSrt = replaceAll (Pattern "\n") (Replacement commentPrefix) str

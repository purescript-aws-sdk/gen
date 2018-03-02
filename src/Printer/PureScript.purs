module Printer.PureScript where

import Prelude
import Data.String (joinWith)
import Data.StrMap (toArrayWithKey)
import Node.Path (FilePath, concat)

import Aws (MetadataElement(MetadataElement), Service(Service))
import Printer.PureScript.Header (header)
import Printer.PureScript.Function (function)
import Printer.PureScript.NewType (newType)

clientFilePath :: FilePath -> MetadataElement -> Service -> FilePath
clientFilePath path (MetadataElement { name }) _ = concat [path, name <> ".purs"]

client :: MetadataElement -> Service -> String
client metadata (Service { operations, shapes, documentation }) =
    (header metadata documentation) <>
    (toArrayWithKey (\name -> \serviceOperation -> function name serviceOperation) operations # joinWith "") <>
    (toArrayWithKey (\name -> \serviceShape -> newType metadata name serviceShape) shapes # joinWith "")

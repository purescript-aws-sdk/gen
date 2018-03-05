module Printer.PureScript.Header where

import Prelude
import Data.Foreign.NullOrUndefined (NullOrUndefined, unNullOrUndefined)
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace, replaceAll)

import Aws (MetadataElement(MetadataElement))
import Printer.PureScript.Comment (comment)

header :: MetadataElement -> NullOrUndefined String -> String
header (MetadataElement { name }) documentation = """
{{documentation}}
module AWS.{{serviceName}} where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "{{serviceName}}" :: String
""" # replaceAll (Pattern "{{serviceName}}") (Replacement name)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment $ unNullOrUndefined documentation)

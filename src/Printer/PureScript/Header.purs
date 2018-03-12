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
import Data.Foreign as Foreign
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types
""" # replace (Pattern "{{serviceName}}") (Replacement name)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment $ unNullOrUndefined documentation)

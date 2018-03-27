module Printer.PureScript.Header where

import Prelude
import Data.Foreign.NullOrUndefined (NullOrUndefined, unNullOrUndefined)
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace, replaceAll)

import AWS (MetadataElement(MetadataElement))
import Printer.PureScript.Comment (comment)

header :: MetadataElement -> NullOrUndefined String -> String
header (MetadataElement { name }) documentation = """
{{documentation}}
module AWS.{{serviceName}} where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap) as StrMap

import AWS.Service (Options, Service, ServiceName(..), service) as AWS
import AWS.Request (MethodName(..), request) as AWS
import AWS.Request.Types (NoArguments(..), NoInput(..), NoOutput(..), Timestamp(..)) as Types

newtype {{serviceName}}Service = {{serviceName}}Service AWS.Service

service :: forall eff. AWS.Options -> Eff (exception :: EXCEPTION | eff) {{serviceName}}Service
service options = do
    let serviceName = AWS.ServiceName "{{serviceName}}"
    service' <- AWS.service serviceName options
    pure $ {{serviceName}}Service service'
""" # replaceAll (Pattern "{{serviceName}}") (Replacement name)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment $ unNullOrUndefined documentation)

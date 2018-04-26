module Printer.PureScript.Module where

import Prelude
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace, replaceAll)

import AWS (MetadataElement(MetadataElement), Service(Service))
import Printer.PureScript.Comment (comment)

fileName :: MetadataElement -> String
fileName (MetadataElement { name }) = name

output :: MetadataElement -> Service -> String
output metadataElement service' =
    (header metadataElement service') <>
    (service metadataElement)

header :: MetadataElement -> Service -> String
header (MetadataElement { name }) (Service { documentation }) = """
{{documentation}}
module AWS.{{name}} where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import AWS.Service (Options, Service, ServiceName(..), service) as AWS
""" # replace (Pattern "{{name}}") (Replacement name)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment documentation)


service :: MetadataElement -> String
service (MetadataElement { name }) = """
newtype Service = Service AWS.Service

service :: forall eff. AWS.Options -> Eff (exception :: EXCEPTION | eff) Service
service options = do
    let serviceName = AWS.ServiceName "{{name}}"
    service' <- AWS.service serviceName options
    pure $ Service service'
""" # replaceAll (Pattern "{{name}}") (Replacement name)

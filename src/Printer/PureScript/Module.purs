module Printer.PureScript.Module where

import Prelude

import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace, replaceAll)
import Printer.PureScript.Comment (comment)
import Printer.Types (ServiceDef)

fileName :: ServiceDef -> String
fileName svc = svc.name

output :: ServiceDef -> String
output svc =
    (header svc) <> (service svc)

header :: ServiceDef -> String
header { name, documentation } = """
{{documentation}}
module AWS.{{name}} where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import AWS.Service (Options, Service, ServiceName(..), service) as AWS
""" # replace (Pattern "{{name}}") (Replacement name)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment documentation)


service :: ServiceDef -> String
service { name } = """
newtype Service = Service AWS.Service

service :: forall eff. AWS.Options -> Eff (exception :: EXCEPTION | eff) Service
service options = do
    let serviceName = AWS.ServiceName "{{name}}"
    service' <- AWS.service serviceName options
    pure $ Service service'
""" # replaceAll (Pattern "{{name}}") (Replacement name)

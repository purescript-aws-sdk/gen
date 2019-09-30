module Printer.PureScript.Requests where

import Prelude

import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), drop, joinWith, replace, replaceAll, take, toLower)
import Printer.PureScript.Comment (comment)
import Printer.Types (ServiceDef, OperationDef)

fileName :: ServiceDef -> String
fileName { name } = name <> "Requests"

output :: ServiceDef -> String
output svc@{ operations } =
    (header svc) <>
    (operations <#> (function svc) # joinWith "")

header :: ServiceDef -> String
header { name } = """
module AWS.{{name}}.Requests where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)

import AWS.Request (MethodName(..), request) as AWS
import AWS.Request.Types as Types

import AWS.{{name}} as {{name}}
import AWS.{{name}}.Types as {{name}}Types
""" # replaceAll (Pattern "{{name}}") (Replacement name)

function :: ServiceDef -> OperationDef -> String
function svc { methodName, input, output: output', documentation } = """
{{documentation}}
{{camelCaseMethodName}} :: forall eff. {{serviceName}}.Service -> {{inputType}} Aff (exception :: EXCEPTION | eff) {{outputType}}
{{camelCaseMethodName}} ({{serviceName}}.Service serviceImpl) = AWS.request serviceImpl method {{inputFallback}} where
    method = AWS.MethodName "{{camelCaseMethodName}}"
""" # replaceAll (Pattern "{{serviceName}}") (Replacement svc.name )
    # replaceAll (Pattern "{{camelCaseMethodName}}") (Replacement camelCaseMethodName)
    # replace (Pattern "{{inputType}}") (Replacement inputType)
    # replace (Pattern "{{inputFallback}}") (Replacement inputFallback)
    # replace (Pattern "{{outputType}}") (Replacement outputType)
    # replace (Pattern "{{documentation}}") (Replacement documentation')
        where
            camelCaseMethodName = (take 1 methodName # toLower) <> (drop 1 methodName)
            inputType = input # maybe "" (\shape -> svc.name <> "Types." <> shape <> " ->")
            inputFallback = input # maybe "unit" (\_ -> "")
            outputType =  output' # maybe "Unit" (\shape -> svc.name <> "Types." <> shape)
            documentation' = documentation # maybe "" comment

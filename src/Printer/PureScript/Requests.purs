module Printer.PureScript.Requests where

import Prelude
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), drop, joinWith, replace, replaceAll, take, toLower)
import Foreign.Object (toArrayWithKey)

import AWS (MetadataElement(MetadataElement), Service(Service), ServiceOperation(ServiceOperation), ServiceShapeName(ServiceShapeName))
import Printer.PureScript.Comment (comment)

fileName :: MetadataElement -> String
fileName (MetadataElement { name }) = name <> "Requests"

output :: MetadataElement -> Service -> String
output metadataElement (Service { operations }) =
    (header metadataElement) <>
    (toArrayWithKey (\name -> \serviceOperation -> function metadataElement name serviceOperation) operations # joinWith "")

header :: MetadataElement -> String
header (MetadataElement { name }) = """
module AWS.{{name}}.Requests where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)

import AWS.Request (MethodName(..), request) as AWS
import AWS.Request.Types as Types

import AWS.{{name}} as {{name}}
import AWS.{{name}}.Types as {{name}}Types
""" # replaceAll (Pattern "{{name}}") (Replacement name)

function :: MetadataElement -> String -> ServiceOperation -> String
function (MetadataElement {name: serviceName}) methodName (ServiceOperation serviceOperation) = """
{{documentation}}
{{camelCaseMethodName}} :: forall eff. {{serviceName}}.Service -> {{inputType}} Aff (exception :: EXCEPTION | eff) {{outputType}}
{{camelCaseMethodName}} ({{serviceName}}.Service serviceImpl) = AWS.request serviceImpl method {{inputFallback}} where
    method = AWS.MethodName "{{camelCaseMethodName}}"
""" # replaceAll (Pattern "{{serviceName}}") (Replacement serviceName)
    # replaceAll (Pattern "{{camelCaseMethodName}}") (Replacement camelCaseMethodName)
    # replace (Pattern "{{inputType}}") (Replacement inputType)
    # replace (Pattern "{{inputFallback}}") (Replacement inputFallback)
    # replace (Pattern "{{outputType}}") (Replacement outputType)
    # replace (Pattern "{{documentation}}") (Replacement documentation)
        where
            camelCaseMethodName = (take 1 methodName # toLower) <> (drop 1 methodName)
            inputType = serviceOperation.input # maybe "" (\(ServiceShapeName { shape }) -> serviceName <> "Types." <> shape <> " ->")
            inputFallback = serviceOperation.input # maybe "unit" (\_ -> "")
            outputType =  serviceOperation.output # maybe "Unit" (\(ServiceShapeName { shape }) -> serviceName <> "Types." <> shape)
            documentation = serviceOperation.documentation # maybe "" comment

module Printer.PureScript.Function where

import Prelude
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), drop, replace, replaceAll, take, toLower)

import Aws (MetadataElement(MetadataElement), ServiceOperation(ServiceOperation), ServiceShapeName(ServiceShapeName))
import Printer.PureScript.Comment (comment)

function :: MetadataElement -> String -> ServiceOperation -> String
function (MetadataElement { name: serviceName }) methodName (ServiceOperation serviceOperation) = """
{{documentation}}
{{camelCaseMethodName}} :: forall eff. {{inputType}} Aff (exception :: EXCEPTION | eff) {{outputType}}
{{camelCaseMethodName}} = Request.request service method {{inputFallback}} where
    service = Request.ServiceName "{{serviceName}}"
    method = Request.MethodName "{{camelCaseMethodName}}"
""" # replace (Pattern "{{serviceName}}") (Replacement serviceName)
    # replaceAll (Pattern "{{camelCaseMethodName}}") (Replacement camelCaseMethodName)
    # replace (Pattern "{{inputType}}") (Replacement inputType)
    # replace (Pattern "{{inputFallback}}") (Replacement inputFallback)
    # replace (Pattern "{{outputType}}") (Replacement outputType)
    # replace (Pattern "{{documentation}}") (Replacement documentation)
        where
            camelCaseMethodName = (take 1 methodName # toLower) <> (drop 1 methodName)
            inputType = unNullOrUndefined serviceOperation.input # maybe "" (\(ServiceShapeName { shape }) -> shape <> " ->")
            inputFallback = unNullOrUndefined serviceOperation.input # maybe "(Types.NoInput unit)" (\_ -> "")
            outputType =  unNullOrUndefined serviceOperation.output # maybe "Types.NoOutput" (\(ServiceShapeName { shape }) -> shape)
            documentation = unNullOrUndefined serviceOperation.documentation # maybe "" comment

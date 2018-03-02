module Printer.PureScript.Function where

import Prelude
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), drop, replace, replaceAll, take, toLower)

import Aws (ServiceOperation(ServiceOperation), ServiceShapeName(ServiceShapeName))
import Printer.PureScript.Comment (comment)

function :: String -> ServiceOperation -> String
function name (ServiceOperation serviceOperation) = """
{{documentation}}
{{camelCaseName}} :: forall eff. {{inputType}} Aff (exception :: EXCEPTION | eff) {{outputType}}
{{camelCaseName}} = Request.request serviceName "{{camelCaseName}}" {{inputFallback}}
""" # replaceAll (Pattern "{{camelCaseName}}") (Replacement camelCaseName)
    # replace (Pattern "{{inputType}}") (Replacement inputType)
    # replace (Pattern "{{inputFallback}}") (Replacement inputFallback)
    # replace (Pattern "{{outputType}}") (Replacement outputType)
    # replace (Pattern "{{documentation}}") (Replacement documentation)
        where
            camelCaseName = (take 1 name # toLower) <> (drop 1 name)
            inputType = unNullOrUndefined serviceOperation.input # maybe "" (\(ServiceShapeName { shape }) -> shape <> " ->")
            inputFallback = unNullOrUndefined serviceOperation.input # maybe "(Types.NoInput unit)" (\_ -> "")
            outputType =  unNullOrUndefined serviceOperation.output # maybe "Types.NoOutput" (\(ServiceShapeName { shape }) -> shape)
            documentation = unNullOrUndefined serviceOperation.documentation # maybe "" comment

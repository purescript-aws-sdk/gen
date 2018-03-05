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
{{camelCaseName}} :: forall eff. {{inputEncoderType}} {{outputDecoderType}} {{inputType}} Aff (exception :: EXCEPTION | eff) {{outputType}}
{{camelCaseName}} {{inputEncoderArg}} {{outputDecoderArg}} = Request.request {{inputEncoder}} {{outputDecoder}} serviceName "{{camelCaseName}}" {{inputFallback}}
""" # replaceAll (Pattern "{{camelCaseName}}") (Replacement camelCaseName)

    # replace (Pattern "{{inputEncoderType}}") (Replacement inputEncoderType)
    # replace (Pattern "{{inputEncoderArg}}") (Replacement inputEncoderArg)
    # replace (Pattern "{{inputEncoder}}") (Replacement inputEncoder)
    # replace (Pattern "{{inputType}}") (Replacement inputType)
    # replace (Pattern "{{inputFallback}}") (Replacement inputFallback)

    # replace (Pattern "{{outputDecoderType}}") (Replacement outputDecoderType)
    # replace (Pattern "{{outputDecoderArg}}") (Replacement outputDecoderArg)
    # replace (Pattern "{{outputDecoder}}") (Replacement outputDecoder)
    # replace (Pattern "{{outputType}}") (Replacement outputType)

    # replace (Pattern "{{documentation}}") (Replacement documentation)
        where
            camelCaseName = (take 1 name # toLower) <> (drop 1 name)

            inputEncoderType = unNullOrUndefined serviceOperation.input # maybe "" (\(ServiceShapeName { shape }) -> "Request.Encode " <> shape <> " ->")
            inputEncoderArg = unNullOrUndefined serviceOperation.input # maybe "" (\_ -> "encode")
            inputEncoder = unNullOrUndefined serviceOperation.input # maybe "Types.encodeNoInput" (\_ -> inputEncoderArg)
            inputType = unNullOrUndefined serviceOperation.input # maybe "" (\(ServiceShapeName { shape }) -> shape <> " ->")
            inputFallback = unNullOrUndefined serviceOperation.input # maybe "Types.noInput" (\_ -> "")

            outputDecoderType = unNullOrUndefined serviceOperation.output # maybe "" (\(ServiceShapeName { shape }) -> "Request.Decode " <> shape <> " ->")
            outputDecoderArg = unNullOrUndefined serviceOperation.output # maybe "" (\_ -> "decode")
            outputDecoder = unNullOrUndefined serviceOperation.output # maybe "Types.decodeNoOutput" (\_ -> outputDecoderArg)
            outputType =  unNullOrUndefined serviceOperation.output # maybe "Unit" (\(ServiceShapeName { shape }) -> shape)

            documentation = unNullOrUndefined serviceOperation.documentation # maybe "" comment

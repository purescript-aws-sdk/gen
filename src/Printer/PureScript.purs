module Printer.PureScript where

import Prelude
import Data.Array (elem)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), drop, dropWhile, joinWith, replace, replaceAll, take, toLower, toUpper)
import Data.StrMap (StrMap, isEmpty, filterKeys, toArrayWithKey)
import Node.Path (FilePath, concat)

import Aws (Service(..), MetadataElement(..), ServiceOperation(..), ServiceShape(..), ServiceShapeName(..))
import Printer.CycledInDeclaration (ServiceName(..), NewTypeName(..), AttributeName(..), notElem)

clientFilePath :: FilePath -> MetadataElement -> Service -> FilePath
clientFilePath path (MetadataElement { name }) _ = concat [path, name <> ".purs"]

client :: MetadataElement -> Service -> String
client metadata (Service { operations, shapes, documentation }) =
    (header metadata documentation) <>
    (toArrayWithKey (\name -> \serviceOperation -> function name serviceOperation) operations # joinWith "") <>
    (toArrayWithKey (\name -> \serviceShape -> record metadata name serviceShape) shapes # joinWith "")

comment :: String -> String
comment str = commentPrefix <> commentedSrt
    where
        commentPrefix = "\n-- | "
        commentedSrt = replaceAll (Pattern "\n") (Replacement commentPrefix) str

header :: MetadataElement -> NullOrUndefined String -> String
header (MetadataElement { name }) documentation = """
{{documentation}}
module AWS.{{serviceName}} where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "{{serviceName}}" :: String
""" # replaceAll (Pattern "{{serviceName}}") (Replacement name)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment $ unNullOrUndefined documentation)

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

purescriptTypes = ["String", "Int", "Number", "Boolean"] :: Array String

compatibleType :: String -> String
compatibleType type' = safeType
    where
        typeNoPrefix = dropWhile (_ == '_') type'
        typePascalCase = (take 1 typeNoPrefix # toUpper) <> (drop 1 typeNoPrefix)
        typeNotJs = case typePascalCase of
            "Blob" -> "String"
            "Integer" -> "Int"
            "Long" -> "Number"
            "Float" -> "Number"
            "Double" -> "Number"
            "Timestamp" -> "Number"

            "Function" -> "Function'"
            "Map" -> "Map'"
            "Record" -> "Record'"
            "Partial" -> "Partial'"
            "Unit" -> "Unit'"

            validType -> validType

        safeType = if (elem typeNotJs purescriptTypes) || (type' == typeNotJs)
            then typeNotJs
            else typeNotJs <> "'"

record :: MetadataElement -> String -> ServiceShape -> String
record metadata name serviceShape = output
    where
        type' = compatibleType name
        output = if (elem type' purescriptTypes)
            then ""
            else record' metadata type' serviceShape

record' :: MetadataElement -> String -> ServiceShape -> String
record' metadata name serviceShape@(ServiceShape { documentation }) = """
{{documentation}}
newtype {{name}} = {{name}} {{type}}
derive instance newtype{{name}} :: Newtype {{name}} _
derive instance repGeneric{{name}} :: Generic {{name}} _
instance show{{name}} :: Show {{name}} where
  show = genericShow
instance decode{{name}} :: Decode {{name}} where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encode{{name}} :: Encode {{name}} where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
""" # replaceAll (Pattern "{{name}}") (Replacement $ name)
    # replace (Pattern "{{type}}") (Replacement $ recordType metadata name serviceShape)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment $ unNullOrUndefined documentation)

recordType :: MetadataElement -> String -> ServiceShape -> String
recordType metadata newTypeName (ServiceShape serviceShape) = case serviceShape of
    { "type": "list", member: NullOrUndefined (Just (shape)) } -> recordArray shape
    { "type": "map", value: NullOrUndefined (Just value) } -> recordMap value
    { "type": "structure", members: NullOrUndefined (Just members), required: NullOrUndefined required } -> recordRecord metadata newTypeName members $ fromMaybe [] required
    { "type": type' } -> compatibleType type'

recordArray :: ServiceShapeName -> String
recordArray (ServiceShapeName { shape }) = "(Array {{type}})"
    # replace (Pattern "{{type}}") (Replacement $ compatibleType shape)

recordMap :: ServiceShapeName -> String
recordMap (ServiceShapeName value) = "(StrMap.StrMap {{value}})"
    # replace (Pattern "{{value}}") (Replacement $ compatibleType value.shape)

recordRecord :: MetadataElement -> String -> StrMap ServiceShapeName -> Array String -> String
recordRecord (MetadataElement { name: serviceName }) newTypeName keyValue required = if isEmpty keyValue
    then "Types.NoArguments"
    else "\n  { {{properties}}\n  }"
        # replace (Pattern "{{properties}}") (Replacement properties)
            where
                notCycledInDeclaration attributeName = notElem
                    (ServiceName serviceName)
                    (NewTypeName newTypeName)
                    (AttributeName attributeName)

                property key (ServiceShapeName { shape }) = "\"{{name}}\" :: {{required}} ({{type}})"
                    # replace (Pattern "{{name}}") (Replacement $ key)
                    # replace (Pattern "{{type}}") (Replacement $ compatibleType shape)
                    # replace (Pattern "{{required}}") (Replacement $ if elem key required then "" else "NullOrUndefined.NullOrUndefined")
                    # replace (Pattern "  ") (Replacement " ")

                properties = filterKeys notCycledInDeclaration keyValue
                    # toArrayWithKey property
                    # joinWith "\n  , "

module Printer.PureScript.Type where

import Prelude
import Data.Array (elem)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), drop, dropWhile, joinWith, replace, replaceAll, take, toUpper)
import Data.StrMap (StrMap, isEmpty, filterKeys, toArrayWithKey)

import Aws (MetadataElement(MetadataElement), ServiceShape(ServiceShape), ServiceShapeName(ServiceShapeName))
import Printer.CycledInDeclaration (ServiceName(..), NewTypeName(..), AttributeName(..), notElem)
import Printer.PureScript.Comment (comment)

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

newType :: MetadataElement -> String -> ServiceShape -> String
newType metadata name serviceShape = output
    where
        type' = compatibleType name
        output = if (elem type' purescriptTypes)
            then ""
            else newType' metadata type' serviceShape

newType' :: MetadataElement -> String -> ServiceShape -> String
newType' metadata name serviceShape@(ServiceShape { documentation }) = """
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
                    # replace (Pattern "{{name}}") (Replacement key)
                    # replace (Pattern "{{type}}") (Replacement $ compatibleType shape)
                    # replace (Pattern "{{required}}") (Replacement $ if elem key required then "" else "NullOrUndefined.NullOrUndefined")
                    # replace (Pattern "  ") (Replacement " ")

                properties = filterKeys notCycledInDeclaration keyValue
                    # toArrayWithKey property
                    # joinWith "\n  , "

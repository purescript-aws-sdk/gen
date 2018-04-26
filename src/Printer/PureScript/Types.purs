module Printer.PureScript.Types where

import Prelude
import Data.Array (elem, partition)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(Pattern), Replacement(..), drop, dropWhile, joinWith, replace, replaceAll, take, toUpper)
import Data.StrMap (StrMap, filterKeys, isEmpty, toArrayWithKey, toAscUnfoldable)
import Data.Tuple (fst, snd)

import AWS (MetadataElement(MetadataElement), Service(Service), ServiceShape(ServiceShape), ServiceShapeName(ServiceShapeName))
import Printer.CycledInDeclaration (ServiceName(..), NewTypeName(..), AttributeName(..), notElem)
import Printer.PureScript.Comment (comment)

fileName :: MetadataElement -> String
fileName (MetadataElement { name }) = name <> "Types"

output :: MetadataElement -> Service -> String
output metadataElement (Service { shapes }) =
    (header metadataElement) <>
    (toArrayWithKey (\name -> \serviceShape -> newType metadataElement name serviceShape) shapes # joinWith "")

header :: MetadataElement -> String
header (MetadataElement { name }) = """
module AWS.{{name}}.Types where

import Prelude
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap) as StrMap

import AWS.Request.Types as Types

options :: Options
options = defaultOptions { unwrapSingleConstructors = true }
""" # replace (Pattern "{{name}}") (Replacement name)

purescriptTypes :: Array String
purescriptTypes =
  [ "Boolean"
  , "Int"
  , "Number"
  , "String"
  , "Types.Timestamp"
  ]

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
            "Timestamp" -> "Types.Timestamp"

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
newType metadata name serviceShape = typeDefinition
    where
        type' = compatibleType name
        typeDefinition = if (elem type' purescriptTypes)
            then ""
            else newType' metadata type' serviceShape

newType' :: MetadataElement -> String -> ServiceShape -> String
newType' metadata name serviceShape@(ServiceShape { documentation }) = """
{{documentation}}
newtype {{name}} = {{name}} {{type}}
derive instance newtype{{name}} :: Newtype {{name}} _
derive instance repGeneric{{name}} :: Generic {{name}} _
instance show{{name}} :: Show {{name}} where show = genericShow
instance decode{{name}} :: Decode {{name}} where decode = genericDecode options
instance encode{{name}} :: Encode {{name}} where encode = genericEncode options
{{defaultConstructor}}
""" # replaceAll (Pattern "{{name}}") (Replacement $ name)
    # replace (Pattern "{{type}}") (Replacement $ recordType metadata name serviceShape)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment documentation)
    # replace (Pattern "{{defaultConstructor}}") (Replacement $ defaultConstructor metadata name serviceShape)

recordType :: MetadataElement -> String -> ServiceShape -> String
recordType metadata newTypeName (ServiceShape serviceShape) = case serviceShape of
    { "type": "list", member: Just shape } -> recordArray shape
    { "type": "map", value: Just value } -> recordMap value
    { "type": "structure", members: Just members, required: required } -> recordRecord metadata newTypeName members $ fromMaybe [] required
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
    else "\n  { {{fields}}\n  }"
        # replace (Pattern "{{fields}}") (Replacement fields)
            where fields = recordFields serviceName newTypeName keyValue required # joinWith "\n  , "

recordFields :: String -> String -> StrMap ServiceShapeName -> Array String -> Array String
recordFields serviceName newTypeName keyValue required = fields
    where field key (ServiceShapeName { shape }) = "\"{{name}}\" :: {{required}} ({{type}})"
              # replace (Pattern "{{name}}") (Replacement key)
              # replace (Pattern "{{type}}") (Replacement $ compatibleType shape)
              # replace (Pattern "{{required}}") (Replacement $ if elem key required then "" else "Maybe")
              # replace (Pattern "  ") (Replacement " ")

          fields = filterKeysNotCycledInDeclaration serviceName newTypeName keyValue # toArrayWithKey field

defaultConstructor :: MetadataElement -> String -> ServiceShape -> String
defaultConstructor metadata newTypeName (ServiceShape serviceShape) = case serviceShape of
    { "type": "structure", members: Just members, required: required } -> defaultRecordConstructor metadata newTypeName members $ fromMaybe [] required
    _ -> ""

defaultRecordConstructor :: MetadataElement -> String -> StrMap ServiceShapeName -> Array String -> String
defaultRecordConstructor (MetadataElement { name: serviceName }) newTypeName keyValue required = if isEmpty keyValue
    then "" -- there's already a singleton constructor
    else """
-- | Constructs {{name}} from required parameters
new{{name}} :: {{newTypeSignature}}
new{{name}} {{arguments}} = {{name}} { {{fieldAssignments}} }

-- | Constructs {{name}}'s fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
new{{name}}' :: {{fieldsSignature}}
new{{name}}' {{arguments}} customize = ({{name}} <<< customize) { {{fieldAssignments}} }
""" # replaceAll (Pattern "{{name}}") (Replacement newTypeName)
    # replaceAll (Pattern "{{newTypeSignature}}") (Replacement newTypeSignature)
    # replaceAll (Pattern "{{fieldsSignature}}") (Replacement fieldsSignature)
    # replaceAll (Pattern "{{arguments}}") (Replacement arguments)
    # replaceAll (Pattern "{{fieldAssignments}}") (Replacement fieldAssignments)
        where newTypeSignature = (signatureTypes <> [newTypeName]) # joinWith " -> "
              fieldsSignature = (signatureTypes <> [ "( { " <> fields <> " } -> {" <> fields <> " } )", newTypeName]) # joinWith " -> "
              fields = recordFields serviceName newTypeName keyValue required # joinWith " , "
              arguments = (escapeArgument <<< fst) <$> requiredFields # joinWith " "
              fieldAssignments = (requiredFieldAssignments <> optionalFieldAssignments) # joinWith ", "
              requiredFieldAssignments = (\f -> escapeFieldName f <> ": " <> escapeArgument f) <$> fst <$> requiredFields
              optionalFieldAssignments = (\f -> escapeFieldName f <> ": Nothing") <$> fst <$> optionalFields
              filteredFields = filterKeysNotCycledInDeclaration serviceName newTypeName keyValue
              signatureTypes = (compatibleType <<< (\(ServiceShapeName{shape}) -> shape) <<< snd) <$> requiredFields
              requiredFields = splitFields.yes
              optionalFields = splitFields.no
              splitFields = partition ((flip elem) required <<< fst) (toAscUnfoldable filteredFields)
              escapeFieldName n = "\"" <> n <> "\""
              escapeArgument n = "_" <> n

filterKeysNotCycledInDeclaration :: String -> String -> StrMap ServiceShapeName -> StrMap ServiceShapeName
filterKeysNotCycledInDeclaration serviceName newTypeName kvs = filterKeys notCycledInDeclaration kvs
    where notCycledInDeclaration attributeName = notElem
              (ServiceName serviceName)
              (NewTypeName newTypeName)
              (AttributeName attributeName)

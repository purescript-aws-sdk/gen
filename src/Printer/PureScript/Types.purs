module Printer.PureScript.Types
       ( fileName
       , output
       ) where

import Prelude

import Data.Array (elem, null, partition)
import Data.Array as Array
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(..), joinWith, replace, replaceAll)
import Printer.CycledInDeclaration (ServiceName(..), NewTypeName(..), AttributeName(..), notElem)
import Printer.PureScript.Comment (comment)
import Printer.Types (ScalarType(..), ServiceDef, ShapeDef, ShapeType(..), StructureMember)

fileName :: ServiceDef -> String
fileName { name } = name <> "Types"

output :: ServiceDef -> String
output serviceDef =
    (header serviceDef) <>
    (serviceDef.shapes <#> newType serviceDef # joinWith "")

header :: ServiceDef -> String
header { name } = """
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

newType :: ServiceDef -> ShapeDef -> String
newType svc shape = """
{{documentation}}
newtype {{name}} = {{name}} {{type}}
derive instance newtype{{name}} :: Newtype {{name}} _
derive instance repGeneric{{name}} :: Generic {{name}} _
instance show{{name}} :: Show {{name}} where show = genericShow
instance decode{{name}} :: Decode {{name}} where decode = genericDecode options
instance encode{{name}} :: Encode {{name}} where encode = genericEncode options
{{defaultConstructor}}
""" # replaceAll (Pattern "{{name}}") (Replacement $ shape.name)
    # replace (Pattern "{{type}}") (Replacement $ recordType svc shape)
    # replace (Pattern "{{documentation}}") (Replacement $ maybe "" comment shape.documentation)
    # replace (Pattern "{{defaultConstructor}}") (Replacement $ defaultConstructor svc shape)

recordType :: ServiceDef -> ShapeDef -> String
recordType svc shape = case shape.shapeType of
    STList { member } -> recordArray member
    STMap { value } -> recordMap value
    STStructure { required, members } -> recordRecord svc shape required members
    STScalar SCBoolean -> "Boolean"
    STScalar SCString -> "String"
    STScalar SCInt -> "Int"
    STScalar SCNumber -> "Number"
    STScalar SCTimestamp -> "Types.Timestamp"
    STUnit -> "Unit"
    STRef type' -> type'

recordArray :: String -> String
recordArray shape = "(Array {{type}})"
    # replace (Pattern "{{type}}") (Replacement shape)

recordMap :: String -> String
recordMap shape = "(StrMap.StrMap {{value}})"
    # replace (Pattern "{{value}}") (Replacement shape)

recordRecord :: ServiceDef -> ShapeDef -> Array String -> Array StructureMember -> String
recordRecord svc shape required members = if null members
    then "Types.NoArguments"
    else "\n  { {{fields}}\n  }"
        # replace (Pattern "{{fields}}") (Replacement fields)
            where fields = recordFields svc shape members required # joinWith "\n  , "

recordFields :: ServiceDef -> ShapeDef -> Array StructureMember -> Array String -> Array String
recordFields svc shape members required = fields
    where field :: StructureMember -> String
          field { name, shapeName } = "\"{{name}}\" :: {{required}} ({{type}})"
              # replace (Pattern "{{name}}") (Replacement name)
              # replace (Pattern "{{type}}") (Replacement shapeName)
              # replace (Pattern "{{required}}") (Replacement $ if elem name required then "" else "Maybe")
              # replace (Pattern "  ") (Replacement " ")

          fields = filterKeysNotCycledInDeclaration svc shape members <#> field

defaultConstructor :: ServiceDef -> ShapeDef -> String
defaultConstructor svc shape = case shape.shapeType of
    STStructure { required, members } -> defaultRecordConstructor svc shape members required
    _ -> ""

defaultRecordConstructor :: ServiceDef -> ShapeDef -> Array StructureMember -> Array String -> String
defaultRecordConstructor svc shape members required = if null members
    then "" -- there's already a singleton constructor
    else """
-- | Constructs {{name}} from required parameters
new{{name}} :: {{newTypeSignature}}
new{{name}} {{arguments}} = {{name}} { {{fieldAssignments}} }

-- | Constructs {{name}}'s fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
new{{name}}' :: {{fieldsSignature}}
new{{name}}' {{arguments}} customize = ({{name}} <<< customize) { {{fieldAssignments}} }
""" # replaceAll (Pattern "{{name}}") (Replacement shape.name)
    # replaceAll (Pattern "{{newTypeSignature}}") (Replacement newTypeSignature)
    # replaceAll (Pattern "{{fieldsSignature}}") (Replacement fieldsSignature)
    # replaceAll (Pattern "{{arguments}}") (Replacement arguments)
    # replaceAll (Pattern "{{fieldAssignments}}") (Replacement fieldAssignments)
        where newTypeSignature = (signatureTypes <> [shape.name]) # joinWith " -> "
              fieldsSignature = (signatureTypes <> [ "( { " <> fields <> " } -> {" <> fields <> " } )", shape.name]) # joinWith " -> "
              fields = recordFields svc shape members required # joinWith " , "
              arguments = (escapeArgument <<< _.name) <$> requiredFields # joinWith " "
              fieldAssignments = (requiredFieldAssignments <> optionalFieldAssignments) # joinWith ", "
              requiredFieldAssignments = (\f -> escapeFieldName f.name <> ": " <> escapeArgument f.name) <$> requiredFields
              optionalFieldAssignments = (\f -> escapeFieldName f.name <> ": Nothing") <$> optionalFields
              filteredFields = filterKeysNotCycledInDeclaration svc shape members
              signatureTypes = (_.shapeName) <$> requiredFields
              requiredFields = splitFields.yes
              optionalFields = splitFields.no
              splitFields = partition ((flip elem) required <<< _.name) filteredFields
              escapeFieldName n = "\"" <> n <> "\""
              escapeArgument n = "_" <> n

filterKeysNotCycledInDeclaration :: ServiceDef -> ShapeDef -> Array StructureMember -> Array StructureMember
filterKeysNotCycledInDeclaration svc shape names = Array.filter notCycledInDeclaration names
    where notCycledInDeclaration { shapeName } = notElem
              (ServiceName svc.name)
              (NewTypeName shape.name)
              (AttributeName shapeName)

module Printer.PureScript.Types
       ( fileName
       , output
       ) where

import Prelude

import Data.Array (null, partition)
import Data.Maybe (maybe)
import Data.String (Pattern(Pattern), Replacement(..), joinWith, replace, replaceAll)
import Printer.PureScript.Comment (comment)
import Printer.Types (MemberType(..), ServiceDef, ShapeDef, ShapeType(..), StructureMember, scalarTypeToPSType)

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
    STStructure { members } -> recordRecord svc shape members

recordArray :: String -> String
recordArray shape = "(Array {{type}})"
    # replace (Pattern "{{type}}") (Replacement shape)

recordMap :: String -> String
recordMap shape = "(StrMap.StrMap {{value}})"
    # replace (Pattern "{{value}}") (Replacement shape)

recordRecord :: ServiceDef -> ShapeDef -> Array StructureMember -> String
recordRecord svc shape members = if null members
    then "Types.NoArguments"
    else "\n  { {{fields}}\n  }"
        # replace (Pattern "{{fields}}") (Replacement fields)
            where fields = recordFields svc shape members # joinWith "\n  , "

recordFields :: ServiceDef -> ShapeDef -> Array StructureMember -> Array String
recordFields svc shape members = fields
    where field :: StructureMember -> String
          field sm = "\"{{name}}\" :: {{type}}"
              # replace (Pattern "{{name}}") (Replacement sm.name)
              # replace (Pattern "{{type}}") (Replacement $ structureMemberToPSType sm)

          fields = members <#> field

structureMemberToPSType :: StructureMember -> String
structureMemberToPSType { memberType, isRequired } =
  isRequiredF $ memberF memberType
  where
    isRequiredF n =
      if isRequired
      then n
      else "Maybe (" <> n <> ")"

    memberF m = case m of
      MTScalar sc -> scalarTypeToPSType sc
      MTRef name -> name
      MTList m' -> "Array (" <> memberF m' <> ")"
      MTMap m' -> "StrMap.StrMap (" <> memberF m' <> ")"

defaultConstructor :: ServiceDef -> ShapeDef -> String
defaultConstructor svc shape = case shape.shapeType of
    STStructure { members } -> defaultRecordConstructor svc shape members

defaultRecordConstructor :: ServiceDef -> ShapeDef -> Array StructureMember -> String
defaultRecordConstructor svc shape members = if null members
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
              fields = recordFields svc shape members # joinWith " , "
              arguments = (escapeArgument <<< _.name) <$> requiredFields # joinWith " "
              fieldAssignments = (requiredFieldAssignments <> optionalFieldAssignments) # joinWith ", "
              requiredFieldAssignments = (\f -> escapeFieldName f.name <> ": " <> escapeArgument f.name) <$> requiredFields
              optionalFieldAssignments = (\f -> escapeFieldName f.name <> ": Nothing") <$> optionalFields
              filteredFields = members
              signatureTypes = structureMemberToPSType <$> requiredFields
              requiredFields = splitFields.yes
              optionalFields = splitFields.no
              splitFields = partition (_.isRequired) filteredFields
              escapeFieldName n = "\"" <> n <> "\""
              escapeArgument n = "_" <> n

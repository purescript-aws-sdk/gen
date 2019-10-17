module AWS.Gen.MetadataReader
       ( ReadError(..)
       , readService
       ) where

import Prelude

import AWS.Gen.Metadata as AWS
import AWS.Gen.Model (MemberType(..), ScalarType(..), ServiceDef, ShapeDef, ShapeType(..), StructureMember, OperationDef)
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either, fromRight)
import Data.Foldable (elem, for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Regex (Regex, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)

data ReadError =
  REMissingShape String
  | REInvalidName String
  | REInvalidOperationType String

derive instance eqReadError :: Eq ReadError
derive instance genReadError :: Generic ReadError _
instance showReadError :: Show ReadError where
  show = genericShow

readService :: AWS.MetadataElement -> AWS.Service -> Either ReadError ServiceDef
readService ameta asvc = do
  shapes <- collectShapes asvc.shapes
  let shapeNames = _.name <$> shapes
  operations <- collectOperations shapeNames asvc.operations
  pure { name: ameta.name
       , documentation: asvc.documentation
       , shapes
       , operations
       }

collectShapes :: Object AWS.ServiceShape -> Either ReadError (Array ShapeDef)
collectShapes ashapes = do
  traverse createShapeDef structures

  where
    structures = Array.catMaybes $ ashapes # Object.toArrayWithKey \name -> case _ of
      { "type": "structure", documentation, members, required } ->
        let members' = members <#> Object.toAscUnfoldable # fromMaybe []
            required' = fromMaybe [] required
        in Just { name, documentation, members: members', required: required' }
      _ ->
        Nothing

    createShapeDef { name, documentation, members, required } = do
      guardName name
      stMembers <- traverse (\(Tuple mName { shape }) -> getStructureMember mName shape (elem mName required) ashapes) members
      pure $ { name, documentation, shapeType: STStructure { members: stMembers } }

collectOperations :: Array String -> Object AWS.ServiceOperation -> Either ReadError (Array OperationDef)
collectOperations shapeNames aops =
  traverse (uncurry createServiceDef) aops'

  where
    aops' = Object.toAscUnfoldable aops

    guardKnownShape n =
      if Array.elem n shapeNames
      then pure unit
      else throwError (REInvalidOperationType n)

    createServiceDef methodName aop = do
      guardName methodName
      let input = aop.input <#> _.shape
      let output = aop.output <#> _.shape
      for_ input guardKnownShape
      for_ output guardKnownShape
      pure { methodName, input, output, documentation: aop.documentation }


guardName :: String -> Either ReadError Unit
guardName s =
  if Regex.test validNameRE s
  then pure unit
  else throwError (REInvalidName s)

validNameRE :: Regex
validNameRE = unsafePartial $ fromRight $ regex "^[a-zA-Z][a-zA-Z0-9]*$" noFlags

getStructureMember :: String -> String -> Boolean -> Object AWS.ServiceShape -> Either ReadError StructureMember
getStructureMember name shapeName isRequired ashapes =
  followRef shapeName ashapes <#> { name, isRequired, memberType: _ }

followRef :: String -> Object AWS.ServiceShape -> Either ReadError MemberType
followRef shapeName ashapes =
  case nameToScalarType shapeName of
    Just sc ->
      pure $ MTScalar sc
    Nothing ->
      case Object.lookup shapeName ashapes of
        Just { "type": "list", member: Just { shape: elShapeName } } ->
          MTList <$> followRef elShapeName ashapes
        Just { "type": "map", value: Just { shape: elShapeName } } ->
          MTMap <$> followRef elShapeName ashapes
        Just { "type": "structure" } ->
          pure $ MTRef shapeName
        Just { "type": shapeName' } ->
          followRef shapeName' ashapes
        Nothing ->
          throwError $ REMissingShape shapeName

safeShapeName :: String -> String
safeShapeName type' = (String.take 1 type' # String.toUpper) <> (String.drop 1 type')

nameToScalarType :: String -> Maybe ScalarType
nameToScalarType "boolean"   = Just SCBoolean
nameToScalarType "blob"      = Just SCString
nameToScalarType "integer"   = Just SCInt
nameToScalarType "long"      = Just SCNumber
nameToScalarType "float"     = Just SCNumber
nameToScalarType "double"    = Just SCNumber
nameToScalarType "string"    = Just SCString
nameToScalarType "timestamp" = Just SCTimestamp
nameToScalarType _           = Nothing

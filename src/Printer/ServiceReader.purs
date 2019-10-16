module Printer.ServiceReader
       ( readService
       ) where

import Prelude

import AWS as AWS
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either, fromRight)
import Data.Foldable (elem, for_)
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
import Printer.Types (MemberType(..), ReadError(..), ScalarType(..), ServiceDef, ShapeDef, ShapeType(..), StructureMember, OperationDef)

readService :: AWS.MetadataElement -> AWS.Service -> Either ReadError ServiceDef
readService (AWS.MetadataElement meta) (AWS.Service svc) = do
  shapes <- collectShapes svc.shapes
  let shapeNames = _.name <$> shapes
  operations <- collectOperations shapeNames svc.operations
  pure { name: meta.name
       , documentation: svc.documentation
       , shapes
       , operations
       }
  where
    toOperation methodName (AWS.ServiceOperation so) =
      { methodName
      , documentation: so.documentation
      , input: so.input <#> unServiceShapeName
      , output: so.output <#> unServiceShapeName
      }

collectShapes :: Object AWS.ServiceShape -> Either ReadError (Array ShapeDef)
collectShapes ashapes = do
  traverse createShapeDef structures

  where
    structures = Array.catMaybes $ ashapes # Object.toArrayWithKey \name -> case _ of
      AWS.ServiceShape { "type": "structure", documentation, members, required } ->
        let members' = members <#> Object.toAscUnfoldable # fromMaybe []
            required' = fromMaybe [] required
        in Just { name, documentation, members: members', required: required' }
      _ ->
        Nothing

    createShapeDef { name, documentation, members, required } = do
      guardName name
      stMembers <- traverse (\(Tuple mName (AWS.ServiceShapeName { shape })) -> getStructureMember mName shape (elem mName required) ashapes) members
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

    createServiceDef methodName (AWS.ServiceOperation so) = do
      guardName methodName
      let input = so.input <#> unServiceShapeName
      let output = so.output <#> unServiceShapeName
      for_ input guardKnownShape
      for_ output guardKnownShape
      pure { methodName, input, output, documentation: so.documentation }


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
      case Object.lookup shapeName ashapes <#> unServiceShape of
        Just { "type": "list", member: Just (AWS.ServiceShapeName { shape: elShapeName }) } ->
          MTList <$> followRef elShapeName ashapes
        Just { "type": "map", value: Just (AWS.ServiceShapeName { shape: elShapeName }) } ->
          MTMap <$> followRef elShapeName ashapes
        Just { "type": "structure" } ->
          pure $ MTRef shapeName
        Just { "type": shapeName' } ->
          followRef shapeName' ashapes
        Nothing ->
          throwError $ REMissingShape shapeName

  where
    unServiceShape (AWS.ServiceShape s) = s

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

unServiceShapeName :: AWS.ServiceShapeName -> String
unServiceShapeName (AWS.ServiceShapeName { shape }) = shape

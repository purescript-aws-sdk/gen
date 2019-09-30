module Printer.ServiceReader
       ( readService
       ) where

import Prelude

import AWS (ServiceShapeName(..))
import AWS as AWS
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Foreign.Object as Object
import Printer.Types (ScalarType(..), ServiceDef, ShapeType(..))

readService :: AWS.MetadataElement -> AWS.Service -> ServiceDef
readService (AWS.MetadataElement meta) (AWS.Service svc) =
  { name: meta.name
  , documentation: svc.documentation
    -- TODO nubbing gets rid of duplicates caused by
    -- cleaning up names (i.e. original name of "boolean" and "Boolean")
    -- assuming they have the same declaration.
    -- but we best look for a safer way to guard against duplicate names
    -- but diff declaration.
  , shapes: Array.sortWith _.name $ Array.nub $ Object.toArrayWithKey toShape svc.shapes
  , operations: Array.sortWith _.methodName $ Object.toArrayWithKey toOperation svc.operations
  }
  where
    toShape name ass@(AWS.ServiceShape ss) =
      { name: safeShapeName name
      , documentation: ss.documentation
      , shapeType: createShapeType ass
      }

    toOperation methodName (AWS.ServiceOperation so) =
      { methodName
      , documentation: so.documentation
      , input: so.input <#> getShapeNameStr
      , output: so.output <#> getShapeNameStr
      }

safeShapeName :: String -> String
safeShapeName type' = (String.take 1 type' # String.toUpper) <> (String.drop 1 type')

createShapeType :: AWS.ServiceShape -> ShapeType
createShapeType (AWS.ServiceShape ss) = case ss of
  { "type": "list", member: Just (ServiceShapeName { shape }) } -> STList { member: safeShapeName shape }
  { "type": "map", value: Just (ServiceShapeName { shape }) } -> STMap { value: safeShapeName shape }
  { "type": "structure", members, required } ->
    let m = maybe [] (Object.toArrayWithKey \name (ServiceShapeName { shape }) -> { name, shapeName: safeShapeName shape }) members
        r = fromMaybe [] required
    in STStructure { members: Array.sortWith _.name m, required: r }
  { "type": type' } -> case nameToScalarType type' of
    Just sc -> STScalar sc
    Nothing -> STRef $ safeShapeName type'

nameToScalarType :: String -> Maybe ScalarType
nameToScalarType "boolean"   = Just SCBoolean
nameToScalarType "blob"      = Just SCString
nameToScalarType "integer"   = Just SCInt
nameToScalarType "long"      = Just SCNumber
nameToScalarType "float"     = Just SCNumber
nameToScalarType "double"    = Just SCNumber
nameToScalarType "timestamp" = Just SCTimestamp
nameToScalarType _           = Nothing

getShapeNameStr :: AWS.ServiceShapeName -> String
getShapeNameStr (AWS.ServiceShapeName { shape }) = shape

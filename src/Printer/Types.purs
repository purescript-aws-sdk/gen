module Printer.Types
       ( ServiceDef
       , ShapeDef
       , ShapeType(..)
       , ScalarType(..)
       , StructureMember
       , OperationDef
       , ReadError(..)
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

type ServiceDef =
  { name :: String
  , documentation :: Maybe String
  , shapes :: Array ShapeDef
  , operations :: Array OperationDef
  }

type ShapeDef =
  { name :: String
  , documentation :: Maybe String
  , shapeType :: ShapeType
  }

data ShapeType =
  STList { member :: String }
  | STMap { value :: String }
  | STStructure { required :: Array String
                , members :: Array StructureMember
                }
  | STScalar ScalarType
  | STRef String
  | STUnit
derive instance eqShapeType :: Eq ShapeType
derive instance ordShapeType :: Ord ShapeType
derive instance genShapeType :: Generic ShapeType _
instance showShapeType :: Show ShapeType where
  show = genericShow

data ScalarType =
  SCString
  | SCInt
  | SCNumber
  | SCBoolean
  | SCTimestamp
derive instance eqScalarType :: Eq ScalarType
derive instance ordScalarType :: Ord ScalarType
derive instance genScalarType :: Generic ScalarType _
instance showScalarType :: Show ScalarType where
  show = genericShow

type StructureMember =
  { name :: String
  , shapeName :: String
  }

type OperationDef =
  { methodName :: String
  , documentation :: Maybe String
  , input :: Maybe String
  , output :: Maybe String
  }

data ReadError

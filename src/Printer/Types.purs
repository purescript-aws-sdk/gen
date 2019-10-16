module Printer.Types
       ( ServiceDef
       , ShapeDef
       , ShapeType(..)
       , MemberType(..)
       , ScalarType(..)
       , scalarTypeToPSType
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
  STStructure { members :: Array StructureMember
              }
derive instance eqShapeType :: Eq ShapeType
derive instance ordShapeType :: Ord ShapeType
derive instance genShapeType :: Generic ShapeType _
instance showShapeType :: Show ShapeType where
  show = genericShow

data MemberType =
  MTScalar ScalarType
  | MTRef String
  | MTList MemberType
  | MTMap MemberType
derive instance eqMemberType :: Eq MemberType
derive instance ordMemberType :: Ord MemberType
derive instance genMemberType :: Generic MemberType _
instance showMemberType :: Show MemberType where
  show x = genericShow x

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

scalarTypeToPSType :: ScalarType -> String
scalarTypeToPSType SCString = "String"
scalarTypeToPSType SCInt = "Int"
scalarTypeToPSType SCNumber = "Number"
scalarTypeToPSType SCBoolean = "Boolean"
scalarTypeToPSType SCTimestamp = "Types.Timestamp"

type StructureMember =
  { name :: String
  , isRequired :: Boolean
  , memberType :: MemberType
  }

type OperationDef =
  { methodName :: String
  , documentation :: Maybe String
  , input :: Maybe String
  , output :: Maybe String
  }

data ReadError =
  REMissingShape String
  | REInvalidName String
  | REInvalidOperationType String

derive instance eqReadError :: Eq ReadError
derive instance genReadError :: Generic ReadError _
instance showReadError :: Show ReadError where
  show = genericShow

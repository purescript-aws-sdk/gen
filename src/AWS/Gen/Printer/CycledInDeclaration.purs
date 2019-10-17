module AWS.Gen.Printer.CycledInDeclaration where

import Prelude (class Eq, not, (==))
import Data.Array as Array
import Data.Tuple.Nested (Tuple3, tuple3)

newtype ServiceName = ServiceName String
instance eqServiceName :: Eq ServiceName where eq (ServiceName a) (ServiceName b) = a == b

newtype NewTypeName = NewTypeName String
instance eqNewTypeName :: Eq NewTypeName where eq (NewTypeName a) (NewTypeName b) = a == b

newtype AttributeName = AttributeName String
instance eqAttributeName :: Eq AttributeName where eq (AttributeName a) (AttributeName b) = a == b

cycleInDeclaration :: Array (Tuple3 ServiceName NewTypeName AttributeName)
cycleInDeclaration = [
    tuple3 (ServiceName "CostExplorer") (NewTypeName "Expression") (AttributeName "Or"),
    tuple3 (ServiceName "CostExplorer") (NewTypeName "Expression") (AttributeName "And"),
    tuple3 (ServiceName "CostExplorer") (NewTypeName "Expression") (AttributeName "Not"),
    tuple3 (ServiceName "DynamoDBStreams") (NewTypeName "AttributeValue") (AttributeName "L"),
    tuple3 (ServiceName "DynamoDBStreams") (NewTypeName "AttributeValue") (AttributeName "M"),
    tuple3 (ServiceName "EMR") (NewTypeName "Configuration") (AttributeName "Configurations"),
    tuple3 (ServiceName "Organizations") (NewTypeName "HandshakeResource") (AttributeName "Resources"),
    tuple3 (ServiceName "SSM") (NewTypeName "InventoryAggregator") (AttributeName "Aggregators")
]

elem :: ServiceName -> NewTypeName -> AttributeName -> Boolean
elem serviceName newTypeName attributeName = Array.elem (tuple3 serviceName newTypeName attributeName) cycleInDeclaration

notElem :: ServiceName -> NewTypeName -> AttributeName -> Boolean
notElem = not elem

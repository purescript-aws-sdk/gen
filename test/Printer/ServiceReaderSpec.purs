module Printer.ServiceReaderSpec
       ( serviceReaderSpec
       ) where

import Prelude

import AWS as AWS
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Printer.ServiceReader (readService)
import Printer.Types (ScalarType(..), ServiceDef, ShapeDef, ShapeType(..), OperationDef)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Row.Homogeneous (class Homogeneous)

type AService =
  { shapes :: Object AWS.ServiceShape
  , operations :: Object AWS.ServiceOperation
  , metadata :: AWS.ServiceMetadata
  , documentation :: Maybe String
  , version :: Maybe String
  }

type AServiceShape =
  { type :: String
  , members ::  Maybe (Object AWS.ServiceShapeName)
  , documentation :: Maybe String
  , required :: Maybe (Array String)
  , member :: Maybe AWS.ServiceShapeName
  , exception :: Maybe Boolean
  , max :: Maybe Number
  , min :: Maybe Number
  , enum :: Maybe (Array String)
  , error :: Maybe AWS.ServiceError
  , pattern :: Maybe String
  , payload :: Maybe String
  , value :: Maybe AWS.ServiceShapeName
  , key :: Maybe AWS.ServiceShapeName
  , wrapper :: Maybe Boolean
  , sensitive :: Maybe Boolean
  , fault :: Maybe Boolean
  , flattened :: Maybe Boolean
  , box :: Maybe Boolean
  , deprecated :: Maybe Boolean
  , streaming :: Maybe Boolean
  , locationName :: Maybe String
  , xmlOrder :: Maybe (Array String)
  , xmlNamespace :: Maybe AWS.ServiceXmlNamespace
  , timestampFormat :: Maybe String
  }

type AOperation =
  { name :: String
  , http :: AWS.ServiceHttp
  , input :: Maybe AWS.ServiceShapeName
  , documentation :: Maybe String
  , errors :: Maybe (Array AWS.ServiceShapeName)
  , output :: Maybe AWS.ServiceShapeName
  , idempotent :: Maybe Boolean
  , documentationUrl :: Maybe String
  , deprecated :: Maybe Boolean
  , authtype :: Maybe String
  , alias :: Maybe String
  }

serviceReaderSpec :: Spec Unit
serviceReaderSpec = do
  describe "Service Reader" do
    it "should copy the service's name" do
      (r identity).name `shouldEqual` "Foo"

    it "should copy the service's documentation" do
      (r _ { documentation = Just "foodoc" }).documentation
        `shouldEqual` (Just "foodoc")

    it "should read scalar shapes" do
      -- sort this by name
      -- to get the same order in the
      -- result
      rshapes { "MyBlob": shape_ "blob"
              , "MyBoolean": shape_ "boolean"
              , "MyDouble": shape_ "double"
              , "MyFloat": shape_ "float"
              , "MyInteger": shape_ "integer"
              , "MyLong": shape_ "long"
              , "MyTimestamp": shape_ "timestamp"
              } `shouldEqual`
        [ shapeDef "MyBlob" $ STScalar SCString
        , shapeDef "MyBoolean" $ STScalar SCBoolean
        , shapeDef "MyDouble" $ STScalar SCNumber
        , shapeDef "MyFloat" $ STScalar SCNumber
        , shapeDef "MyInteger" $ STScalar SCInt
        , shapeDef "MyLong" $ STScalar SCNumber
        , shapeDef "MyTimestamp" $ STScalar SCTimestamp
        ]

    it "should read a list shape" do
      rshapes { "MyList": shape "list" _ { member = jsname "MyInteger" }
              } `shouldEqual`
        [ shapeDef "MyList" $ STList { member: "MyInteger" }
        ]

    it "should read a map shape" do
      rshapes { "MyMap": shape "map" _ { value = jsname "MyInteger" }
              } `shouldEqual`
        [ shapeDef "MyMap" $ STMap { value: "MyInteger" }
        ]

    it "should read a structure shape" do
      rshapes { "MyStructure": shape "structure" _
                { required = Just [ "A", "B" ]
                , members = Just $ Object.fromHomogeneous
                  { "A": sname "SA"
                  , "B": sname "SB"
                  , "C": sname "SC"
                  }
                }
              } `shouldEqual`
        [ shapeDef "MyStructure" $ STStructure
          { required: [ "A", "B" ]
          , members:
            [ { name: "A", shapeName: "SA" }
            , { name: "B", shapeName: "SB" }
            , { name: "C", shapeName: "SC" }
            ]
          }
        ]

    it "should read operations with input and output" do
      roperations { "Op1": operation "Op1" _ { input = jsname "Op1Input"
                                             , output = jsname "Op1Output"
                                             }
                  , "Op2": operation "Op2" _ { input = Nothing
                                             , output = Nothing
                                             }
                  } `shouldEqual`
        [ operationDef "Op1" (Just "Op1Input") (Just "Op1Output")
        , operationDef "Op2" Nothing Nothing
        ]

r :: (AService -> AService) -> ServiceDef
r f = readService meta (svc f)

rshapes :: forall r. Homogeneous r AWS.ServiceShape => { |r } -> Array ShapeDef
rshapes shapes' = (r _ { shapes = Object.fromHomogeneous shapes'}).shapes

roperations :: forall r. Homogeneous r AWS.ServiceOperation => { |r } -> Array OperationDef
roperations ops' = (r _ { operations = Object.fromHomogeneous ops' }).operations

-- Creators

meta :: AWS.MetadataElement
meta =
  AWS.MetadataElement { name: "Foo"
                      , prefix: Nothing
                      }

svc :: (AService -> AService) -> AWS.Service
svc f = AWS.Service $ f emptyASvc

shape :: String -> (AServiceShape -> AServiceShape) -> AWS.ServiceShape
shape typ f = AWS.ServiceShape $ f $ emptyAShape typ

shape_ :: String -> AWS.ServiceShape
shape_ typ = shape typ identity

sname :: String -> AWS.ServiceShapeName
sname shape' = AWS.ServiceShapeName { shape: shape' }

jsname :: String -> Maybe AWS.ServiceShapeName
jsname = Just <<< sname

operation :: String -> (AOperation -> AOperation) -> AWS.ServiceOperation
operation name f = AWS.ServiceOperation $ f $ emptyAOperation name

shapeDef :: String -> ShapeType -> ShapeDef
shapeDef name shapeType =
  { name
  , documentation: Nothing
  , shapeType
  }

operationDef :: String -> Maybe String -> Maybe String -> OperationDef
operationDef methodName input output =
  { methodName
  , documentation: Nothing
  , input
  , output
  }

-- Empty

emptyMetadata :: AWS.ServiceMetadata
emptyMetadata = AWS.ServiceMetadata
  { signatureVersion: ""
  , serviceFullName: ""
  , protocol: ""
  , endpointPrefix: ""
  , apiVersion: ""
  , uid: Nothing
  , jsonVersion: Nothing
  , targetPrefix: Nothing
  , serviceAbbreviation: Nothing
  , serviceId: Nothing
  , signingName: Nothing
  , xmlNamespace: Nothing
  , globalEndpoint: Nothing
  , timestampFormat: Nothing
  , checksumFormat: Nothing
  }


emptyASvc :: AService
emptyASvc =
  { shapes: Object.empty
  , operations: Object.empty
  , metadata: emptyMetadata
  , documentation: Nothing
  , version: Nothing
  }

emptyAShape :: String -> AServiceShape
emptyAShape type' =
  { "type": type'
  , members: Nothing
  , documentation: Nothing
  , required: Nothing
  , member: Nothing
  , exception: Nothing
  , max: Nothing
  , min: Nothing
  , enum: Nothing
  , error: Nothing
  , pattern: Nothing
  , payload: Nothing
  , value: Nothing
  , key: Nothing
  , wrapper: Nothing
  , sensitive: Nothing
  , fault: Nothing
  , flattened: Nothing
  , box: Nothing
  , deprecated: Nothing
  , streaming: Nothing
  , locationName: Nothing
  , xmlOrder: Nothing
  , xmlNamespace: Nothing
  , timestampFormat: Nothing
  }

emptyAOperation :: String -> AOperation
emptyAOperation name =
  { name
  , http: AWS.ServiceHttp { method: "", requestUri: "" }
  , input: Nothing
  , documentation: Nothing
  , errors: Nothing
  , output: Nothing
  , idempotent: Nothing
  , documentationUrl: Nothing
  , deprecated: Nothing
  , authtype: Nothing
  , alias: Nothing
  }

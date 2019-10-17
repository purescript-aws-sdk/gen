module AWS.Gen.MetadataReaderSpec
       ( metadataReaderSpec
       ) where

import Prelude

import AWS.Gen.Metadata as AWS
import AWS.Gen.MetadataReader (ReadError(..), readService)
import AWS.Gen.Model (MemberType(..), OperationDef, ScalarType(..), ServiceDef, ShapeDef, ShapeType(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (throwError)
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)
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

metadataReaderSpec :: Spec Unit
metadataReaderSpec = do
  describe "Service Reader" do
    it "should copy the service's name" do
      s <- r' identity
      s.name `shouldEqual` "Foo"

    it "should copy the service's documentation" do
      s <- r' _ { documentation = Just "foodoc" }
      s.documentation `shouldEqual` (Just "foodoc")

    -- shapes

    it "should skip non structure shapes" do
      -- sort this by name
      -- to get the same order in the
      -- result
      s <- r { "MyBlob": shape_ "blob"
             , "MyBoolean": shape_ "boolean"
             , "MyDouble": shape_ "double"
             , "MyFloat": shape_ "float"
             , "MyInteger": shape_ "integer"
             , "MyLong": shape_ "long"
             , "MyString": shape_ "string"
             , "MyTimestamp": shape_ "timestamp"

             , "MyList": shape "list" _ { member = jsname "integer" }
             , "MyMap": shape "map" _ { value = jsname "integer" }
             } {}
      s.shapes `shouldEqual` []

    it "should read a structure of scalars" do
      s <- r { "MyStructure": shape "structure" _
                { required = Just []
                , members = Just $ Object.fromHomogeneous
                  { "Blob": sname "blob"
                  , "Boolean": sname "boolean"
                  , "Double": sname "double"
                  , "Float": sname "float"
                  , "Integer": sname "integer"
                  , "Long": sname "long"
                  , "String": sname "string"
                  , "Timestamp": sname "timestamp"
                  }
                }
             } {}
      s.shapes `shouldEqual`
        [ shapeDef "MyStructure" $ STStructure
          { members:
            [ { name: "Blob", isRequired: false, memberType: MTScalar SCString }
            , { name: "Boolean", isRequired: false, memberType: MTScalar SCBoolean }
            , { name: "Double", isRequired: false, memberType: MTScalar SCNumber }
            , { name: "Float", isRequired: false, memberType: MTScalar SCNumber }
            , { name: "Integer", isRequired: false, memberType: MTScalar SCInt }
            , { name: "Long", isRequired: false, memberType: MTScalar SCNumber }
            , { name: "String", isRequired: false, memberType: MTScalar SCString }
            , { name: "Timestamp", isRequired: false, memberType: MTScalar SCTimestamp }
            ]
          }
        ]

    it "should trace the references" do
      s <- r { "MyStructure": shape "structure" _
                { members = Just $ Object.fromHomogeneous
                  { "Boolean0": sname "boolean"
                  , "Boolean1": sname "MyBoolean1"
                  , "Boolean2": sname "MyBoolean2"
                  }
                }
             , "MyBoolean1": shape_ "boolean"
             , "MyBoolean2": shape_ "MyBoolean1"
             } {}
      s.shapes `shouldEqual`
        [ shapeDef "MyStructure" $ STStructure
          { members:
            [ { name: "Boolean0", isRequired: false, memberType: MTScalar SCBoolean }
            , { name: "Boolean1", isRequired: false, memberType: MTScalar SCBoolean }
            , { name: "Boolean2", isRequired: false, memberType: MTScalar SCBoolean }
            ]
          }
        ]

    it "should trace structure refernces" do
      s <- r { "MyStructure0": shape_ "structure"
             , "MyStructure1": shape "structure" _
               { members = Just $ Object.fromHomogeneous
                 { "A": sname "MyStructure0"
                 }
               }
             } {}
      s.shapes `shouldEqual`
        [ shapeDef "MyStructure0" $ STStructure { members: [] }
        , shapeDef "MyStructure1" $ STStructure
          { members:
            [ { name: "A", isRequired: false, memberType: MTRef "MyStructure0" }
            ]
          }
        ]

    it "should track required members" do
      s <- r { "MyStructure": shape "structure" _
                { required = Just [ "R" ]
                , members = Just $ Object.fromHomogeneous
                  { "R": sname "boolean"
                  }
                }
             } {}
      let firstMemberReq = case s.shapes # Array.head <#> _.shapeType of
            Just (STStructure {members}) -> Array.head members <#> _.isRequired
            _ -> Nothing

      firstMemberReq `shouldContain` true

    it "should read a list structure member" do
      s <- r { "MyStructure": shape "structure" _
                { required = Just []
                , members = Just $ Object.fromHomogeneous
                  { "Booleans": sname "MyBooleans"
                  }
                }
             , "MyBooleans": shape "list" _ { member = jsname "boolean" }
             } {}
      s.shapes `shouldEqual`
        [ shapeDef "MyStructure" $ STStructure
          { members:
            [ { name: "Booleans", isRequired: false, memberType: MTList (MTScalar SCBoolean) }
            ]
          }
        ]

    it "should read a map structure member" do
      s <- r { "MyStructure": shape "structure" _
                { required = Just []
                , members = Just $ Object.fromHomogeneous
                  { "Booleans": sname "MyBooleans"
                  }
                }
             , "MyBooleans": shape "map" _ { value = jsname "boolean" }
             } {}
      s.shapes `shouldEqual`
        [ shapeDef "MyStructure" $ STStructure
          { members:
            [ { name: "Booleans", isRequired: false, memberType: MTMap (MTScalar SCBoolean) }
            ]
          }
        ]

    it "should guard against invalid shape name"
      let guardInvalidName name = do
            e <- rErr' (_ { shapes = Object.fromFoldable
                                     [ Tuple name (shape_ "structure")
                                     ]
                          })
            e `shouldEqual` (REInvalidName name)
      in traverse_ guardInvalidName
         [ "_foo"
         , "1foo"
         , "foo!"
         ]

    it "should fail when referring to an unknown shape" do
      err <- rErr { "MyStructure": shape "structure" _
                       { members = Just $ Object.fromHomogeneous
                                   { "A": sname "Foo"
                                   }
                       }
                  } {}
      err `shouldEqual` (REMissingShape "Foo")

    -- Operations

    it "should read operations with input and output" do
      s <- r { "Op1Input": shape_ "structure"
             , "Op1Output": shape_ "structure"
             }
             { "Op1": operation "Op1" _ { input = jsname "Op1Input"
                                        , output = jsname "Op1Output"
                                        }
             , "Op2": operation "Op2" _ { input = Nothing
                                        , output = Nothing
                                        }
             }
      s.operations `shouldEqual`
        [ operationDef "Op1" (Just "Op1Input") (Just "Op1Output")
        , operationDef "Op2" Nothing Nothing
        ]

    it "should use key as operation name" do
      s <- r {}
             { "Op1": operation "OpA" identity
             }
      (s.operations # Array.head <#> _.methodName) `shouldContain` "Op1"

    it "should guard against invalid operation name"
      let guardInvalidName name = do
            e <- rErr' (_ { operations =
                               Object.fromFoldable
                               [ Tuple name (operation name identity)
                               ]
                          })
            e `shouldEqual` (REInvalidName name)
      in traverse_ guardInvalidName
         [ "_foo"
         , "1foo"
         , "foo!"
         ]

    it "should guard against operation names with missing input shape" do
      e <- rErr {} { "Op1": operation "Op1" _ { input = jsname "Op1Input"
                                              }
                   }
      e `shouldEqual` (REInvalidOperationType "Op1Input")


    it "should guard against operation names with non structure input/output" do
      e <- rErr { "Op1Input": shape_ "integer"
                }
                { "Op1": operation "Op1" _ { input = jsname "Op1Input"
                                           }
                }
      e `shouldEqual` (REInvalidOperationType "Op1Input")

r' :: forall m. MonadThrow Error m => (AService -> AService) -> m ServiceDef
r' f = case readService meta (svc f) of
  Left l -> throwError <<< error $ "unable to build service def: " <> show l
  Right s -> pure s

r
  :: forall m r1 r2
     . MonadThrow Error m
     => Homogeneous r1 AWS.ServiceShape
     => Homogeneous r2 AWS.ServiceOperation
     => { |r1 }
     -> { |r2 }
     -> m ServiceDef
r shapes' operations' =
  r' _ { shapes = Object.fromHomogeneous shapes'
       , operations = Object.fromHomogeneous operations'
       }

rErr' :: forall m. MonadThrow Error m => (AService -> AService) -> m ReadError
rErr' f = case readService meta (svc f) of
  Left l -> pure l
  Right s -> throwError <<< error $ "expected read error"

rErr
  :: forall m r1 r2
     . MonadThrow Error m
     => Homogeneous r1 AWS.ServiceShape
     => Homogeneous r2 AWS.ServiceOperation
     => { |r1 }
     -> { |r2 }
     -> m ReadError
rErr shapes' operations' =
  rErr' _ { shapes = Object.fromHomogeneous shapes'
          , operations = Object.fromHomogeneous operations'
          }

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

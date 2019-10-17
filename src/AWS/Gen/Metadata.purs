module AWS.Gen.Metadata where

import Prelude
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (ignoreCase)
import Foreign.Object (Object)

type Metadata = Object MetadataElement

type MetadataElement =
  { name :: String
  , prefix :: Maybe String
  }

metadataFileRegex :: MetadataElement -> Either String Regex
metadataFileRegex element = pattern where
  prefix = fromMaybe element.name element.prefix
  pattern = regex (prefix <> "-[0-9]{4}-[0-9]{2}-[0-9]{2}.normal.json") ignoreCase

type Service =
  { shapes :: Object ServiceShape
  , operations :: Object ServiceOperation
  , metadata :: ServiceMetadata
  , documentation :: Maybe String
  , version :: Maybe String
  }

type ServiceMetadata =
  { signatureVersion :: String
  , serviceFullName :: String
  , protocol :: String
  , endpointPrefix :: String
  , apiVersion :: String
  , uid :: Maybe String
  , jsonVersion :: Maybe String
  , targetPrefix :: Maybe String
  , serviceAbbreviation :: Maybe String
  , serviceId :: Maybe String
  , signingName :: Maybe String
  , xmlNamespace :: Maybe String
  , globalEndpoint :: Maybe String
  , timestampFormat :: Maybe String
  , checksumFormat :: Maybe String
  }

type ServiceOperation =
  { name :: String
  , http :: ServiceHttp
  , input :: Maybe ServiceShapeName
  , documentation :: Maybe String
  , errors :: Maybe (Array ServiceShapeName)
  , output :: Maybe ServiceShapeName
  , idempotent :: Maybe Boolean
  , documentationUrl :: Maybe String
  , deprecated :: Maybe Boolean
  , authtype :: Maybe String
  , alias :: Maybe String
  }

type ServiceShape =
  { type :: String
  , members ::  Maybe (Object ServiceShapeName)
  , documentation :: Maybe String
  , required :: Maybe (Array String)
  , member :: Maybe ServiceShapeName
  , exception :: Maybe Boolean
  , max :: Maybe Number
  , min :: Maybe Number
  , enum :: Maybe (Array String)
  , error :: Maybe ServiceError
  , pattern :: Maybe String
  , payload :: Maybe String
  , value :: Maybe ServiceShapeName
  , key :: Maybe ServiceShapeName
  , wrapper :: Maybe Boolean
  , sensitive :: Maybe Boolean
  , fault :: Maybe Boolean
  , flattened :: Maybe Boolean
  , box :: Maybe Boolean
  , deprecated :: Maybe Boolean
  , streaming :: Maybe Boolean
  , locationName :: Maybe String
  , xmlOrder :: Maybe (Array String)
  , xmlNamespace :: Maybe ServiceXmlNamespace
  , timestampFormat :: Maybe String
  }

type ServiceShapeName = { shape :: String }

type ServiceHttp = { method :: String, requestUri :: String }

type ServiceError =
  { httpStatusCode :: Int
  , code :: Maybe String
  , senderFault :: Maybe Boolean
  }

type ServiceXmlNamespace = { uri :: String }

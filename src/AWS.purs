module AWS where

import Prelude
import Data.Either (Either)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.NullOrUndefined (NullOrUndefined, unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.StrMap (StrMap)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (ignoreCase)

options :: Options
options = defaultOptions { unwrapSingleConstructors = true }

newtype Metadata = Metadata (StrMap MetadataElement)

derive instance repGenericMetadata :: Generic Metadata _
instance decodeMetadata :: Decode Metadata where decode = genericDecode options
instance encodeMetadata :: Encode Metadata where encode = genericEncode options

newtype MetadataElement = MetadataElement
  { name :: String
  , prefix :: NullOrUndefined String
  }

derive instance repGenericMetadataElement :: Generic MetadataElement _
instance decodeMetadataElement :: Decode MetadataElement where decode = genericDecode options
instance encodeMetadataElement :: Encode MetadataElement where encode = genericEncode options

metadataFileRegex :: MetadataElement -> Either String Regex
metadataFileRegex (MetadataElement element) = pattern where
  prefix = fromMaybe element.name $ unNullOrUndefined element.prefix
  pattern = regex (prefix <> "-[0-9]{4}-[0-9]{2}-[0-9]{2}.normal.json") ignoreCase

newtype Service = Service
  { shapes :: StrMap ServiceShape
  , operations :: StrMap ServiceOperation
  , metadata :: ServiceMetadata
  , documentation :: NullOrUndefined String
  , version :: NullOrUndefined String
  }

derive instance repGenericService :: Generic Service _
instance decodeService :: Decode Service where decode = genericDecode options
instance encodeService :: Encode Service where encode = genericEncode options

newtype ServiceMetadata = ServiceMetadata
  { signatureVersion :: String
  , serviceFullName :: String
  , protocol :: String
  , endpointPrefix :: String
  , apiVersion :: String
  , uid :: NullOrUndefined String
  , jsonVersion :: NullOrUndefined String
  , targetPrefix :: NullOrUndefined String
  , serviceAbbreviation :: NullOrUndefined String
  , serviceId :: NullOrUndefined String
  , signingName :: NullOrUndefined String
  , xmlNamespace :: NullOrUndefined String
  , globalEndpoint :: NullOrUndefined String
  , timestampFormat :: NullOrUndefined String
  , checksumFormat :: NullOrUndefined String
  }

derive instance repGenericServiceMetadata :: Generic ServiceMetadata _
instance decodeServiceMetadata :: Decode ServiceMetadata where decode = genericDecode options
instance encodeServiceMetadata :: Encode ServiceMetadata where encode = genericEncode options

newtype ServiceOperation = ServiceOperation
  { name :: String
  , http :: ServiceHttp
  , input :: NullOrUndefined ServiceShapeName
  , documentation :: NullOrUndefined String
  , errors :: NullOrUndefined (Array ServiceShapeName)
  , output :: NullOrUndefined ServiceShapeName
  , idempotent :: NullOrUndefined Boolean
  , documentationUrl :: NullOrUndefined String
  , deprecated :: NullOrUndefined Boolean
  , authtype :: NullOrUndefined String
  , alias :: NullOrUndefined String
  }

derive instance repGenericServiceOperation :: Generic ServiceOperation _
instance decodeServiceOperation :: Decode ServiceOperation where decode = genericDecode options
instance encodeServiceOperation :: Encode ServiceOperation where encode = genericEncode options

newtype ServiceShape = ServiceShape
  { type :: String
  , members ::  NullOrUndefined (StrMap ServiceShapeName)
  , documentation :: NullOrUndefined String
  , required :: NullOrUndefined (Array String)
  , member :: NullOrUndefined ServiceShapeName
  , exception :: NullOrUndefined Boolean
  , max :: NullOrUndefined Number
  , min :: NullOrUndefined Number
  , enum :: NullOrUndefined (Array String)
  , error :: NullOrUndefined ServiceError
  , pattern :: NullOrUndefined String
  , payload :: NullOrUndefined String
  , value :: NullOrUndefined ServiceShapeName
  , key :: NullOrUndefined ServiceShapeName
  , wrapper :: NullOrUndefined Boolean
  , sensitive :: NullOrUndefined Boolean
  , fault :: NullOrUndefined Boolean
  , flattened :: NullOrUndefined Boolean
  , box :: NullOrUndefined Boolean
  , deprecated :: NullOrUndefined Boolean
  , streaming :: NullOrUndefined Boolean
  , locationName :: NullOrUndefined String
  , xmlOrder :: NullOrUndefined (Array String)
  , xmlNamespace :: NullOrUndefined ServiceXmlNamespace
  , timestampFormat :: NullOrUndefined String
  }

derive instance repGenericServiceShape :: Generic ServiceShape _
instance decodeServiceShape :: Decode ServiceShape where decode = genericDecode options
instance encodeServiceShape :: Encode ServiceShape where encode = genericEncode options

newtype ServiceShapeName = ServiceShapeName { shape :: String }
derive instance repGenericServiceShapeName :: Generic ServiceShapeName _
instance decodeServiceShapeName :: Decode ServiceShapeName where decode = genericDecode options
instance encodeServiceShapeName :: Encode ServiceShapeName where encode = genericEncode options

newtype ServiceHttp = ServiceHttp { method :: String, requestUri :: String }
derive instance repGenericServiceHttp :: Generic ServiceHttp _
instance decodeServiceHttp :: Decode ServiceHttp where decode = genericDecode options
instance encodeServiceHttp :: Encode ServiceHttp where encode = genericEncode options

newtype ServiceError = ServiceError
  { httpStatusCode :: Int
  , code :: NullOrUndefined String
  , senderFault :: NullOrUndefined Boolean
  }

derive instance repGenericServiceError :: Generic ServiceError _
instance decodeServiceError :: Decode ServiceError where decode = genericDecode options
instance encodeServiceError :: Encode ServiceError where encode = genericEncode options

newtype ServiceXmlNamespace = ServiceXmlNamespace { uri :: String }
derive instance repGenericServiceXmlNamespace :: Generic ServiceXmlNamespace _
instance decodeServiceXmlNamespace :: Decode ServiceXmlNamespace where decode = genericDecode options
instance encodeServiceXmlNamespace :: Encode ServiceXmlNamespace where encode = genericEncode options

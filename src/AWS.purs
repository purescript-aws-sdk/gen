module AWS where

import Prelude
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (ignoreCase)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (Options, defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object)

options :: Options
options = defaultOptions { unwrapSingleConstructors = true }

newtype Metadata = Metadata (Object MetadataElement)

derive instance repGenericMetadata :: Generic Metadata _
instance decodeMetadata :: Decode Metadata where decode = genericDecode options
instance encodeMetadata :: Encode Metadata where encode = genericEncode options

newtype MetadataElement = MetadataElement
  { name :: String
  , prefix :: Maybe String
  }

derive instance repGenericMetadataElement :: Generic MetadataElement _
instance decodeMetadataElement :: Decode MetadataElement where decode = genericDecode options
instance encodeMetadataElement :: Encode MetadataElement where encode = genericEncode options

metadataFileRegex :: MetadataElement -> Either String Regex
metadataFileRegex (MetadataElement element) = pattern where
  prefix = fromMaybe element.name element.prefix
  pattern = regex (prefix <> "-[0-9]{4}-[0-9]{2}-[0-9]{2}.normal.json") ignoreCase

newtype Service = Service
  { shapes :: Object ServiceShape
  , operations :: Object ServiceOperation
  , metadata :: ServiceMetadata
  , documentation :: Maybe String
  , version :: Maybe String
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

derive instance repGenericServiceMetadata :: Generic ServiceMetadata _
instance decodeServiceMetadata :: Decode ServiceMetadata where decode = genericDecode options
instance encodeServiceMetadata :: Encode ServiceMetadata where encode = genericEncode options

newtype ServiceOperation = ServiceOperation
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

derive instance repGenericServiceOperation :: Generic ServiceOperation _
instance decodeServiceOperation :: Decode ServiceOperation where decode = genericDecode options
instance encodeServiceOperation :: Encode ServiceOperation where encode = genericEncode options

newtype ServiceShape = ServiceShape
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
  , code :: Maybe String
  , senderFault :: Maybe Boolean
  }

derive instance repGenericServiceError :: Generic ServiceError _
instance decodeServiceError :: Decode ServiceError where decode = genericDecode options
instance encodeServiceError :: Encode ServiceError where encode = genericEncode options

newtype ServiceXmlNamespace = ServiceXmlNamespace { uri :: String }
derive instance repGenericServiceXmlNamespace :: Generic ServiceXmlNamespace _
instance decodeServiceXmlNamespace :: Decode ServiceXmlNamespace where decode = genericDecode options
instance encodeServiceXmlNamespace :: Encode ServiceXmlNamespace where encode = genericEncode options

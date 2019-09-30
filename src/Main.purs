module Main where

import Prelude

import AWS (Metadata(Metadata), MetadataElement(..), metadataFileRegex)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (test)
import Data.Traversable (find, traverse_)
import Eff (liftExcept)
import Effect (Effect)
import Effect.Aff (Aff, apathize, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FS (mkdirRecursive)
import Foreign.Generic (decodeJSON)
import Foreign.Object (values)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, writeTextFile)
import Node.Path (FilePath, dirname)
import Node.Path as Path
import Printer.PureScript (filePath, project)
import Printer.PureScript.Module as ModulePrinter
import Printer.PureScript.Requests as RequestsPrinter
import Printer.PureScript.Types as TypesPrinter
import Printer.ServiceReader (readService)
import Printer.Types (ServiceDef)

apisMetadataFilePath = "./aws-sdk-js/apis/metadata.json" :: FilePath
apisPath = "./aws-sdk-js/apis/" :: FilePath

clientsPath = "aws-sdk-purs" :: FilePath

createClientProject :: FilePath -> ServiceDef -> Aff Unit
createClientProject path svc = project path svc

createClientFiles :: FilePath -> ServiceDef -> Aff Unit
createClientFiles path svc = do
  let filePath' = filePath path svc
  _ <- apathize $ mkdirRecursive $ dirname $ filePath' ""

  let moduleFilePath = filePath' $ ModulePrinter.fileName svc
  let moduleContent = ModulePrinter.output svc
  _ <- writeTextFile UTF8 moduleFilePath moduleContent

  let requestsFilePath = filePath' $ RequestsPrinter.fileName svc
  let requestsContent = RequestsPrinter.output svc
  _ <- writeTextFile UTF8 requestsFilePath requestsContent

  let typesFilePath = filePath' $ TypesPrinter.fileName svc
  let typesContent = TypesPrinter.output svc
  writeTextFile UTF8 typesFilePath typesContent

runProject :: Array FilePath -> FilePath -> MetadataElement -> Aff Unit
runProject apiFileNames clientsProject metadataElement = do
  log $ "Creating project - " <> name
  fileNameRegex <- mkApiFileNameRegex -- can be simplified later on not to use regex
  fileName <- findFileName fileNameRegex
  awsService <- readAwsService $ Path.concat [ apisPath, fileName ]
  serviceDef <- readServiceDef awsService
  createClientProject clientsPath serviceDef
  createClientFiles clientsPath serviceDef
  where
    name = case metadataElement of
      (MetadataElement { name: name' }) -> name'

    mkApiFileNameRegex = case metadataFileRegex metadataElement of
      Right r -> pure r
      Left l -> throwError (error "Unable to form regex")

    findFileName pattern = case find (test pattern) apiFileNames of
      Just r -> pure r
      Nothing -> throwError (error $ "Unable to find file of pattern: " <> show pattern)

    readAwsService filePath = do
      jsonString <- readTextFile UTF8 filePath
      decodeJSON jsonString # liftExcept # liftEffect

    readServiceDef awsService = pure $ readService metadataElement awsService

main :: Effect Unit
main = launchAff_ do
  apiMetadataFileContent <- readTextFile UTF8 apisMetadataFilePath
  Metadata metadata <- decodeJSON apiMetadataFileContent # liftExcept # liftEffect
  let metadataElements = values metadata
  apiFileNames <- readdir apisPath

  traverse_ (runProject apiFileNames clientsPath) metadataElements
  liftEffect $ log "Done!"

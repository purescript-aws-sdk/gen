module Main where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex, test)
import Data.Traversable (find, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, apathize, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Generic (decodeJSON)
import Foreign.Object (values)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, writeTextFile)
import Node.Path (FilePath, concat, dirname)

import AWS (Metadata(Metadata), MetadataElement, Service, metadataFileRegex)
import Eff (liftEither, liftExcept, liftMaybe)
import Printer.PureScript (filePath, project)
import Printer.PureScript.Module as ModulePrinter
import Printer.PureScript.Requests as RequestsPrinter
import Printer.PureScript.Types as TypesPrinter
import FS (mkdirRecursive)

apisMetadataFilePath = "./aws-sdk-js/apis/metadata.json" :: FilePath
apisPath = "./aws-sdk-js/apis/" :: FilePath

clientsPath = "aws-sdk-purs" :: FilePath

metadataWithApiFileRegex :: MetadataElement -> Either String (Tuple MetadataElement Regex)
metadataWithApiFileRegex metadata = metadataFileRegex metadata
  # map (\pattern -> Tuple metadata pattern)

metadataWithApiFileName :: Array FilePath -> Tuple MetadataElement Regex -> Maybe (Tuple MetadataElement FilePath)
metadataWithApiFileName fileNames (Tuple metadata pattern) = find (test pattern) fileNames
  # map (\fileName -> Tuple metadata fileName)

metadataWithApiFilePath :: FilePath -> Tuple MetadataElement FilePath -> Tuple MetadataElement FilePath
metadataWithApiFilePath path (Tuple metadata fileName) = Tuple metadata (concat [path, fileName])

metadataWithService :: Tuple MetadataElement String -> Aff (Tuple MetadataElement Service)
metadataWithService (Tuple metadata filePath) = do
  jsonString <- readTextFile UTF8 filePath
  service <- decodeJSON jsonString # liftExcept # liftEffect
  pure $ Tuple metadata service

createClientProject :: FilePath -> Tuple MetadataElement Service -> Aff Unit
createClientProject path (Tuple metadata _) = project path metadata

createClientFiles :: FilePath -> Tuple MetadataElement Service -> Aff Unit
createClientFiles path (Tuple metadata service) = do
  let filePath' = filePath path metadata
  _ <- apathize $ mkdirRecursive $ dirname $ filePath' ""

  let moduleFilePath = filePath' $ ModulePrinter.fileName metadata
  let moduleContent = ModulePrinter.output metadata service
  _ <- writeTextFile UTF8 moduleFilePath moduleContent

  let requestsFilePath = filePath' $ RequestsPrinter.fileName metadata
  let requestsContent = RequestsPrinter.output metadata service
  _ <- writeTextFile UTF8 requestsFilePath requestsContent

  let typesFilePath = filePath' $ TypesPrinter.fileName metadata
  let typesContent = TypesPrinter.output metadata service
  writeTextFile UTF8 typesFilePath typesContent

main :: Effect (Fiber Unit)
main = launchAff do
  apiMetadataFileContent <- readTextFile UTF8 apisMetadataFilePath
  Metadata metadata <- decodeJSON apiMetadataFileContent # liftExcept # liftEffect
  let metadataElements = values metadata

  metadataElementsWithApiFileRegex <- map metadataWithApiFileRegex metadataElements
    # traverse (liftEither >>> liftEffect)

  apiFileNames <- readdir apisPath
  metadataElementsWithApiFileName <- map (metadataWithApiFileName apiFileNames) metadataElementsWithApiFileRegex
    # traverse (liftMaybe >>> liftEffect)

  let metadataElementsWithApiFilePaths = map (metadataWithApiFilePath apisPath) metadataElementsWithApiFileName
  metadataElementsWithServices <- traverse metadataWithService metadataElementsWithApiFilePaths
  _ <- traverse (createClientProject clientsPath) metadataElementsWithServices
  _ <- traverse (createClientFiles clientsPath) metadataElementsWithServices

  liftEffect $ log "Hello sailor!"

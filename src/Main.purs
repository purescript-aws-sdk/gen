module Main where

import Prelude (Unit, bind, map, pure, (#), ($), (>>>))
import Control.Monad.Aff (Aff, Fiber, apathize, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either)
import Data.Foreign.Generic (decodeJSON)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex, test)
import Data.StrMap (values)
import Data.Traversable (find, traverse)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, readTextFile, readdir, writeTextFile)
import Node.Path (FilePath, concat, dirname)

import AWS (Metadata(Metadata), MetadataElement, Service, metadataFileRegex)
import Eff (liftEither, liftExcept, liftMaybe)
import Printer.PureScript (client, clientFilePath, project)
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

metadataWithService :: forall eff. Tuple MetadataElement String -> Aff (exception :: EXCEPTION, fs :: FS | eff) (Tuple MetadataElement Service)
metadataWithService (Tuple metadata filePath) = do
  jsonString <- readTextFile UTF8 filePath
  service <- decodeJSON jsonString # liftExcept # liftEff
  pure $ Tuple metadata service

createClientProject :: forall eff. FilePath -> Tuple MetadataElement Service -> Aff (fs :: FS | eff) Unit
createClientProject path (Tuple metadata service) = project path metadata service

createClientFile :: forall eff. FilePath -> Tuple MetadataElement Service -> Aff (fs :: FS | eff) Unit
createClientFile path (Tuple metadata service) = do
  let filePath = clientFilePath path metadata service
  let fileContent = client metadata service

  _ <- apathize $ mkdirRecursive $ dirname filePath
  writeTextFile UTF8 filePath fileContent

main :: forall eff. Eff (fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff) (Fiber (fs :: FS, exception :: EXCEPTION , console :: CONSOLE | eff) Unit)
main = launchAff do
  apiMetadataFileContent <- readTextFile UTF8 apisMetadataFilePath
  Metadata metadata <- decodeJSON apiMetadataFileContent # liftExcept # liftEff
  let metadataElements = values metadata

  metadataElementsWithApiFileRegex <- map metadataWithApiFileRegex metadataElements
    # traverse (liftEither >>> liftEff)

  apiFileNames <- readdir apisPath
  metadataElementsWithApiFileName <- map (metadataWithApiFileName apiFileNames) metadataElementsWithApiFileRegex
    # traverse (liftMaybe >>> liftEff)

  let metadataElementsWithApiFilePaths = map (metadataWithApiFilePath apisPath) metadataElementsWithApiFileName
  metadataElementsWithServices <- traverse metadataWithService metadataElementsWithApiFilePaths
  _ <- traverse (createClientProject clientsPath) metadataElementsWithServices
  _ <- traverse (createClientFile clientsPath) metadataElementsWithServices

  liftEff $ log "Hello sailor!"

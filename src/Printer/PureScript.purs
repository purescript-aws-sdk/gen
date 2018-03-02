module Printer.PureScript where

import Prelude (Unit, bind, map, pure, unit, (#), ($), (<>))
import Control.Monad.Aff (Aff, apathize)
import Control.Parallel (parTraverse)
import Data.String (Pattern(Pattern), Replacement(Replacement), joinWith, replace, replaceAll, toLower)
import Data.StrMap (toArrayWithKey)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, readTextFile, writeTextFile)
import Node.Path (FilePath, concat)

import Aws (MetadataElement(MetadataElement), Service(Service))
import FS (PartitionPaths(..), mkdirRecursive, partitionPaths, readdirRecursive)
import Printer.PureScript.Header (header)
import Printer.PureScript.Function (function)
import Printer.PureScript.NewType (newType)

projectTemplatePath = "resources/templates/purescript/project" :: FilePath

clientProjectPath :: FilePath -> MetadataElement -> Service -> FilePath
clientProjectPath path (MetadataElement { name }) _ = concat [path, "purescript-aws-" <> (toLower name)]

clientFilePath :: FilePath -> MetadataElement -> Service -> FilePath
clientFilePath path (metadata@MetadataElement { name }) service = concat [clientProjectPath path metadata service, "src", name <> ".purs"]

client :: MetadataElement -> Service -> String
client metadata (Service { operations, shapes, documentation }) =
    (header metadata documentation) <>
    (toArrayWithKey (\name -> \serviceOperation -> function name serviceOperation) operations # joinWith "") <>
    (toArrayWithKey (\name -> \serviceShape -> newType metadata name serviceShape) shapes # joinWith "")

project :: forall eff. FilePath -> MetadataElement -> Service -> Aff(fs :: FS | eff) Unit
project path metadata service  = do
    let projectPath = clientProjectPath path metadata service

    paths <- readdirRecursive projectTemplatePath
    PartitionPaths { directoryPaths, filePaths } <- partitionPaths paths
    filePathsAndContent <- parTraverse (\f -> readTextFile UTF8 f # map (\c -> Tuple f c)) filePaths
    let filePathsAndNewContent = map (\(Tuple f c) -> Tuple f $ updateFileContent metadata c) filePathsAndContent
    let newFilePathsAndNewContent = map (\(Tuple f c) -> Tuple (updateFilePath projectPath f) c) filePathsAndNewContent
    let newDirectoryPaths = map (updateFilePath projectPath) directoryPaths

    _ <- apathize $ parTraverse (mkdirRecursive) newDirectoryPaths
    _ <- parTraverse (\(Tuple f c) -> writeTextFile UTF8 f c) newFilePathsAndNewContent
    pure unit

updateFileContent :: MetadataElement -> String -> String
updateFileContent (MetadataElement { name }) str = str
    # replaceAll (Pattern "{{MODULE_NAME_LOWER}}") (Replacement $ toLower name)

updateFilePath :: FilePath -> FilePath -> FilePath
updateFilePath path filePath = replace (Pattern projectTemplatePath) (Replacement path) filePath

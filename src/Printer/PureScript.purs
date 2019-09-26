module Printer.PureScript where

import Prelude (Unit, bind, map, pure, unit, (#), ($), (<>))
import Data.String (Pattern(Pattern), Replacement(Replacement), replace, replaceAll, toLower)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, apathize)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath, concat)

import AWS (MetadataElement(MetadataElement))
import FS (PartitionPaths(..), mkdirRecursive, partitionPaths, readdirRecursive)

projectTemplatePath = "resources/templates/purescript/project" :: FilePath

projectPath :: FilePath -> MetadataElement -> FilePath
projectPath path (MetadataElement { name }) = concat [path, "purescript-aws-" <> (toLower name)]

filePath :: FilePath -> MetadataElement -> String -> FilePath
filePath path metadata@(MetadataElement { name }) fileName = concat [projectPath path metadata, "src", fileName <> ".purs"]

project :: FilePath -> MetadataElement -> Aff Unit
project path metadata  = do
    let projectPath' = projectPath path metadata

    paths <- readdirRecursive projectTemplatePath
    PartitionPaths { directoryPaths, filePaths } <- partitionPaths paths
    filePathsAndContent <- traverse (\f -> readTextFile UTF8 f # map (\c -> Tuple f c)) filePaths
    let filePathsAndNewContent = map (\(Tuple f c) -> Tuple f $ updateFileContent metadata c) filePathsAndContent
    let newFilePathsAndNewContent = map (\(Tuple f c) -> Tuple (updateFilePath projectPath' f) c) filePathsAndNewContent
    let newDirectoryPaths = map (updateFilePath projectPath') directoryPaths

    _ <- apathize $ traverse (mkdirRecursive) newDirectoryPaths
    _ <- traverse (\(Tuple f c) -> writeTextFile UTF8 f c) newFilePathsAndNewContent
    pure unit

updateFileContent :: MetadataElement -> String -> String
updateFileContent (MetadataElement { name }) str = str
    # replaceAll (Pattern "{{MODULE_NAME_LOWER}}") (Replacement $ toLower name)

updateFilePath :: FilePath -> FilePath -> FilePath
updateFilePath path filePath' = replace (Pattern projectTemplatePath) (Replacement path) filePath'

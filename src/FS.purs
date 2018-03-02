module FS where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Parallel (parTraverse)
import Data.Array (concat, cons, partition)
import Data.Tuple (Tuple(..))
import Node.FS.Aff (FS, exists, mkdir, readdir, stat)
import Node.FS.Stats (isDirectory)
import Node.Path as Path --(FilePath, concat, dirname)
import Node.Path (FilePath, dirname)

newtype PartitionPaths = PartitionPaths
    { directoryPaths:: Array FilePath
    , filePaths:: Array FilePath
    }

partitionPaths :: forall eff. Array FilePath -> Aff (fs :: FS | eff) PartitionPaths
partitionPaths paths = do
    pathStats <- parTraverse (\p -> stat p # map (\s -> Tuple p s)) paths
    let { yes: directoryStats, no: fileStats } = partition (\(Tuple p s) -> isDirectory s) pathStats
    let directoryPaths = map (\(Tuple p s) -> p) directoryStats
    let filePaths = map (\(Tuple p s) -> p) fileStats

    pure (PartitionPaths { directoryPaths, filePaths })

readdirFullPath :: forall eff. FilePath -> Aff (fs :: FS | eff) (Array FilePath)
readdirFullPath path = do
    names <- readdir path
    pathStat <- stat path
    let directoryName = if isDirectory pathStat
            then path
            else dirname path

    let paths = map (\name -> Path.concat [directoryName, name]) names
    pure paths

readdirRecursive :: forall eff. FilePath -> Aff (fs :: FS | eff) (Array FilePath)
readdirRecursive path = do
    paths <- readdirFullPath path
    PartitionPaths { directoryPaths, filePaths } <- partitionPaths paths
    otherPaths <- parTraverse readdirRecursive directoryPaths
    let all = cons filePaths otherPaths # cons directoryPaths # concat
    pure all

mkdirRecursive :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
mkdirRecursive path = do
    let pathParent = dirname path
    parentExists <- exists pathParent
    _ <- if not parentExists
        then mkdirRecursive pathParent
        else pure unit

    mkdir path

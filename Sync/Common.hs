module Sync.Common where

import Control.Monad.State
import qualified Data.Map as M
import System.INotify

import System.Log.Logger as E

type SyncState = StateT Sync IO ()
type FileStructureFactory = FilePath -> StateT Sync IO (FileStructure)

data Sync = Sync {
    getMap  :: M.Map FilePath FileStructure,
    getFileStructureFactory :: FileStructureFactory,
    getBasePath :: FilePath
}

data FileStructure = FileStructure {
    getName :: FilePath,
    getChildren :: [FilePath], 
    getWatchDescriptor :: WatchDescriptor
}

instance Show FileStructure where
    show a = show $ "[Node: " ++ getName a ++ 
                    " children: " ++ (show $ getChildren a) ++ "]"

data FileEvent = FileEvent {
    getPath :: FilePath,
    getEvent :: Event
} deriving (Show)

logName :: String
logName = "Sync"

l :: String -> IO ()
l msg = debugM logName msg


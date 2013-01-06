module Sync.Events(
    fileEventCallback,
    handleFileEvents
    ) where

import Sync.Export
import Sync.RSync
import Sync.DirWatch

import Data.Map as M

fileEventCallback :: Chan FileEvent -> FilePath -> Event -> IO ()
fileEventCallback eventChan monitoredDir e = do
    writeChan eventChan fileEvent
  where 
    fileEvent = FileEvent { 
        getPath = monitoredDir,
        getEvent = e
    }

handleFileEvents :: FilePath -> [FileEvent] -> SyncState
handleFileEvents destPath ((FileEvent watchedPath e):fs) = do
    lift $ l $ "handleFileEvents. event: " ++ (show e)
    handleFileEvent destPath watchedPath e
    handleFileEvents destPath fs

handleFileEvents _ [] = return ()

handleFileEvent :: FilePath -> FilePath -> Event -> SyncState
handleFileEvent destPath watchedPath (Closed isDir (Just fileName) writeable) = do
    basePath <- gets getBasePath
    if not isDir && writeable
        then lift $ syncFile basePath filePath destPath
        else return ()
  where
    filePath = watchedPath </> fileName

handleFileEvent destPath watchedPath (Deleted isDir fileName) = do
    handleDeletion watchedPath destPath isDir fileName
    m <- gets getMap
    lift $ l $ "After deleted. map state: " ++ show m

handleFileEvent destPath watchedPath (MovedOut isDir fileName _) = do
    handleDeletion watchedPath destPath isDir fileName
    m <- gets getMap
    lift $ l $ "After movedout. map state: " ++ show m

handleFileEvent destPath watchedPath (Created isDir fileName) =
    handleCreation watchedPath destPath isDir fileName

handleFileEvent destPath watchedPath (MovedIn isDir fileName _) = 
    handleCreation watchedPath destPath isDir fileName

handleFileEvent _ _ e = 
    lift $ l $ "ignoring: " ++ show e




handleCreation :: FilePath -> FilePath -> Bool -> FilePath -> SyncState
handleCreation watchedPath destPath isDir fileName = do
    basePath <- gets getBasePath
    if isDir
        then do
            fsMap <- gets getMap
            let parent = lookupFileStructure watchedPath fsMap
            addWatchesForDirectory filePath parent
            lift $ syncDir basePath filePath destPath
        else
            lift $ syncFile basePath filePath destPath
  where
    filePath = watchedPath </> fileName 

handleDeletion :: FilePath -> FilePath -> Bool -> FilePath -> SyncState
handleDeletion watchedPath destPath isDir fileName = do
    if isDir 
        then deleteWatchesForDirectory filePath
        else return ()
    state <- get
    lift $ syncDir (getBasePath state) watchedPath destPath
  where
    filePath = watchedPath </> fileName

lookupFileStructure :: FilePath -> M.Map FilePath FileStructure -> FileStructure
lookupFileStructure watchedPath m = do
    fromMaybe (error errStr) (M.lookup watchedPath m)
  where
    errStr = printf "lookupFileStructure. map: %s, watchedPath %s\n"
                    (show m) watchedPath
module Sync.DirWatch(
    addWatchesForRoot,
    addWatchesForDirectory,
    deleteWatchesForDirectory
    ) where

import Sync.Export

import qualified Data.Map as M
import qualified System.Posix.Files as P

-- | Adds

addWatchesForRoot :: SyncState
addWatchesForRoot = do
    state <- get
    let root = getBasePath state
    children <- lift $ getDirectoryChildren root
    node <- (getFileStructureFactory state) root
    addWatchesForContents children node

addWatchesForDirectory :: FilePath -> FileStructure -> SyncState
addWatchesForDirectory dir parent =
    if elem dir (getChildren parent) 
        then error errStr
        else do
            state <- get

            children <- lift $ getDirectoryChildren dir
            node <- (getFileStructureFactory state) dir

            -- dir is now a child of parent
            let m1 = M.insert (getName parent) 
                              parent { getChildren = dir : getChildren parent } 
                              (getMap state)
                m2 = M.insert dir
                              node 
                              m1

            put $ state { getMap = m2 }
            addWatchesForContents children node
  where
    errStr = printf "addWatchesForDirectory. parent: %s, dir: %s\n" (show parent) dir

getDirectoryChildren :: FilePath -> IO [FilePath]
getDirectoryChildren dir = do
    contents <- getDirectoryContents dir
    filterFolderNames contents dir

addWatchesForContents :: [FilePath] -> FileStructure -> SyncState
addWatchesForContents (c:cs) node = do 
    fileStatus <- lift $ P.getFileStatus c
    if P.isDirectory fileStatus
        then do
            addWatchesForDirectory c node
            addWatchesForContents cs node
        else addWatchesForContents cs node
addWatchesForContents [] _ = return ()

filterFolderNames :: [FilePath] -> FilePath -> IO [FilePath]
filterFolderNames (p:ps) basePath = do
    isDir <- doesDirectoryExist folderPath
    if not (elem p ["..", "."]) && isDir
        then do
            l $ printf "is dir folderPath: %s" folderPath
            t <- (filterFolderNames ps basePath)
            return $  folderPath : t
        else filterFolderNames ps basePath
  where
    folderPath = basePath </> p
filterFolderNames [] _ = return []

-- | Deletes

deleteWatchesForDirectory :: FilePath -> SyncState
deleteWatchesForDirectory dir = do
    lift $ l $ "deleteWatchesForDirectory. path: " ++ dir

    state <- get
    runMaybeT $ do
        fs <- (MaybeT . return) $ M.lookup dir $ getMap state
        lift $ lift $ removeWatch (getWatchDescriptor fs) -- to IO
        lift $ mapM_ deleteWatchesForDirectory $ getChildren fs
    put $ state { getMap = M.delete dir (getMap state) }


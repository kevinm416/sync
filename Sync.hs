module Main where

import Sync.Config
import Sync.Events
import Sync.Export
import Sync.DirWatch
import Sync.RSync

import qualified Data.Map as M

makeFileStructureFactory :: INotify -> [EventVariety] -> Chan FileEvent -> FileStructureFactory
makeFileStructureFactory inotify watchedEvents eventChanHandle =
    \watchDir -> do
        let prod = fileEventCallback eventChanHandle watchDir
        wd <- lift $ addWatch
            inotify
            watchedEvents
            watchDir
            prod
        state <- get
        let ret =  FileStructure {
                getName = watchDir,
                getChildren = [],
                getWatchDescriptor = wd
            }
        put state { getMap = M.insert watchDir ret (getMap state) }
        return ret

-- watchedEvents = [Close, Delete, Create, MoveIn, MoveOut]
watchedEvents = [AllEvents]

main = do
    initializeLogging
    l "initialized logging"

    config <- parseConfig
    l $ "parsed config: " ++ show config

    inotify <- initINotify
    l "initialized inotify"

    eventChanHandle <- newChan
    eventChan <- getChanContents eventChanHandle

    let fsFactory = makeFileStructureFactory
            inotify 
            watchedEvents 
            eventChanHandle

        base = getBase config
        dst = getDestination config
        src = getSource config

        startState = Sync {
            getMap = M.empty,
            getFileStructureFactory = fsFactory,
            getBasePath = base
        }

    runStateT (do
        addWatchesForRoot
        lift $ l "added root watches"

        lift $ syncDir base src dst
        lift $ l $ "uploaded initial directory structure"

        fileMap <- gets getMap
        lift $ l $ "initial file structure: " ++ show fileMap

        handleFileEvents dst eventChan
        ) startState

initializeLogging = do
    updateGlobalLogger logName (setLevel DEBUG)
    -- s <- fileHandler "Sync.log" DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s)    

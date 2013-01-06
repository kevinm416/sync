module Sync.RSync(
    syncDir,
    syncFile
    ) where

import Sync.Export

combinedOptions = " --delete --archive --verbose --compress "

syncCombined :: FilePath -> FilePath -> String -> IO ()
syncCombined srcPathWithSep adjDestPath rsyncOpts = do
    if (pathBlocked srcPathWithSep)
        then l $ "blocked: " ++ srcPathWithSep
        else do
            l $ "cmd: " ++ cmd
            handleExitCode srcPathWithSep cmd
  where
    cmd = printf "rsync %s -e ssh \"%s\" \"%s\"" 
                 rsyncOpts 
                 srcPathWithSep 
                 adjDestPath

handleExitCode :: FilePath -> String -> IO ()
handleExitCode srcPathWithSep cmd = do
    exitCode <- system cmd
    case exitCode of 
        ExitFailure x -> errorM logName $ errStr x
        ExitSuccess   -> l $ "uploaded: " ++ srcPathWithSep
  where 
    errStr x = "exit code: " ++ (show x) ++ " cmd: " ++ cmd

syncDir :: FilePath -> FilePath -> FilePath -> IO ()
syncDir basePath srcPath destPath = do
    adjDestPath <- getAdjustedDestPath basePath srcPath destPath
    syncCombined srcPathWithSlash adjDestPath dirOptions
  where
    srcPathWithSlash = srcPath ++ [pathSeparator]
    dirOptions = combinedOptions ++ " --cvs-exclude "

syncFile :: FilePath -> FilePath -> FilePath -> IO ()
syncFile basePath srcPath destPath = do
    adjDestPath <- getAdjustedDestPath basePath srcPath destPath
    syncCombined srcPath adjDestPath combinedOptions

getAdjustedDestPath :: FilePath -> FilePath -> FilePath -> IO FilePath
getAdjustedDestPath basePath srcPath destPath = 
    return $ destPath ++ (fromMaybe (error errStr) (stripPrefix basePath srcPath) )
  where
    errStr = printf "getAdjustedDestPath. basePath: %s, srcPath: %s, destPath: %s"
                    basePath srcPath destPath

pathBlocked _ = False
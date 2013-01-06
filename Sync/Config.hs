{-# LANGUAGE OverloadedStrings #-}

module Sync.Config (
    Options,
    getSource,
    getDestination,
    getBase,
    parseConfig
    ) where

import Sync.Export

import Options.Applicative as Opt
import Data.Configurator as Cfg
import System.Directory(getCurrentDirectory, canonicalizePath)
import System.Environment(getEnv)

data Options = Options {
    getSource :: String,
    getDestination :: String,
    getBase :: String
} deriving (Show)

data Options' = Options' {
    getSrc :: Maybe String,
    getDst :: Maybe String,
    getCfg :: String
} deriving (Show)

parseConfig :: IO Options
parseConfig = do
    cmdLineOptions <- Opt.execParser opts
    cfgOptions <- Cfg.load [Optional $ getCfg cmdLineOptions]

    cfgSource <- Cfg.lookup cfgOptions "source"
    cfgDestination <- Cfg.lookup cfgOptions "destination"

    let config = cmdLineOptions {
        getSrc = selectOption (getSrc cmdLineOptions) cfgSource,
        getDst = selectOption (getDst cmdLineOptions) cfgDestination
    }

    validateOptions config
  where
    opts = info (helper <*> configParser) fullDesc

selectOption :: Maybe String -> Maybe String -> Maybe String
selectOption a@(Just cmdLineOption) _  = a
selectOption Nothing cfgOption = cfgOption

validateOptions :: Options' -> IO Options
validateOptions (Options' Nothing _ _) = error "Error: source empty"
validateOptions (Options' (Just _) Nothing _) = error "Error: destination empty"
validateOptions (Options' (Just src) (Just dst) _) = do
    src' <- canonicalizePath src
    base <- readBasePath
    return Options {
        getSource = src',
        getDestination = dst,
        getBase = base
    }

parseOpt :: String -> Either ParseError (Maybe String)
parseOpt = Right . Just

configParser :: Opt.Parser Options'
configParser = Options'
    <$> option
        ( long "source"
        & short 's'
        & value Nothing
        & reader parseOpt
        & metavar "SOURCE"
        & help "Files that will be synced" )
    <*> option
        ( long "destination"
        & short 'd'
        & value Nothing
        & reader parseOpt
        & metavar "DESTINATION"
        & help "Where files will be synced to" )
    <*> strOption
        ( long "config"
        & short 'c'
        & value ".sync"
        & metavar "CONFIG"
        & help "Location of the config file" )

readBasePath :: IO FilePath
readBasePath = canonicalizePath =<< expandTildeInPath =<< getCurrentDirectory

expandTildeInPath :: FilePath -> IO FilePath
expandTildeInPath path =
    if head path == '~' 
        then do  home <- getEnv "HOME"
                 return $ home ++ tail path 
        else return path
{-# LANGUAGE OverloadedStrings #-}

module RemoteLoader where

import Network.HTTP.Simple (httpJSON, getResponseStatusCode, getResponseHeader, getResponseBody, httpBS)
import Data.Aeson (Value, decode)
import Data.Aeson.Types (Value(..))
import Connection (DatedGame)
import System.Environment (getExecutablePath)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (readFile', openFile, IOMode (ReadMode), hGetContents)
import Control.Exception (try)


getGameDataFP :: IO FilePath
getGameDataFP = do
    appDataDir <- getAppUserDataDirectory "haskellconnections"
    let fp = appDataDir </> "connectiongames.txt"
    createDirectoryIfMissing True appDataDir
    return fp

-- fetches gamedata from github
fetchGames :: IO ()
fetchGames = do 
    response <- httpJSON "https://raw.githubusercontent.com/Eyefyre/NYT-Connections-Answers/refs/heads/main/connections.json" 
    let dgames = getResponseBody response :: [DatedGame]
    gdFP <- getGameDataFP
    writeFile gdFP (show dgames) 

getGameData :: IO [DatedGame]
getGameData = do
    gdFP <- getGameDataFP
    dataStr <- either (const "[]") id <$> (try (readFile gdFP) :: IO (Either IOError String))
    return $ read dataStr
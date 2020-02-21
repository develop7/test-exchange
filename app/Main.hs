{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8      (pack)
import           Data.Maybe                 (fromMaybe)
import           Database.PostgreSQL.Simple (ConnectInfo (..),
                                             connectPostgreSQL,
                                             defaultConnectInfo)
import           System.Environment         (getEnv, lookupEnv)
import           Text.Read                  (readMaybe)

import           WebApp                     (runApp)

main :: IO ()
main = do
  port <- fromMaybe "3000" <$> lookupEnv "PORT"
  pgURI <- fromMaybe "postgres:///cweb_exchange" <$> lookupEnv "DATABASE_URL"
  conn <- connectPostgreSQL $ pack pgURI
  runApp (read port) conn

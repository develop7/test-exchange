{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8      (pack)
import           Data.Maybe                 (fromMaybe)
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           System.Environment         (lookupEnv)

import           WebApp                     (runApp)

main :: IO ()
main = do
  port <- fromMaybe "3000" <$> lookupEnv "PORT"
  pgURI <- fromMaybe "postgres:///cweb_exchange" <$> lookupEnv "DATABASE_URL"
  conn <- connectPostgreSQL $ pack pgURI
  runApp (read port) conn

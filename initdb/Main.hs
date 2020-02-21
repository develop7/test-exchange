{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8            (pack)
import           Data.Maybe                       (fromMaybe)
import           Database.PostgreSQL.Simple       (connectPostgreSQL)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           System.Environment               (lookupEnv)

import           Model
import           Queries

main :: IO ()
main = do
  pgURI <- fromMaybe "postgres:///cweb_exchange" <$> lookupEnv "DATABASE_URL"
  conn <- connectPostgreSQL $ pack pgURI

  dropUsers conn
  createUsers conn

  mapM_ (addUser conn) users
  where
    users = [User "john" 100 100, User "ripley" 200 200, User "drummer" 300 300]

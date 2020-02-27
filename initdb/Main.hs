{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8            (pack)
import           Data.Maybe                       (fromMaybe)
import           Database.PostgreSQL.Simple       (Connection,
                                                   connectPostgreSQL,
                                                   withTransaction)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           System.Environment               (lookupEnv)

import           Model
import           Queries

main :: IO ()
main = do
  pgURI <- fromMaybe "postgres:///cweb_exchange" <$> lookupEnv "DATABASE_URL"
  conn <- connectPostgreSQL $ pack pgURI
  dropOrders conn
  dropUsers conn
  createUsers conn
  createOrders conn
  mapM_ (importer conn) users
  where
    importer :: Connection -> (User, [Order]) -> IO ()
    importer conn (user, orders) = do
      withTransaction conn $ do
        id <- addUser conn user
        mapM_ (addOrder conn id) orders
      pure ()
    users =
      [ (User "john" 100 100, [LimitOrder Buy USD 10 1.5, LimitOrder Sell EUR 5 1])
      , (User "ripley" 200 200, [])
      , (User "drummer" 300 300, [])
      ]

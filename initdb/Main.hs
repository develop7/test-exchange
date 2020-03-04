{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8      (pack)
import           Data.Maybe                 (fromMaybe)
import           Data.Scientific            (Scientific)
import           Data.Time.Clock            (getCurrentTime)
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL, withTransaction)
import           System.Environment         (lookupEnv)

import           Model
import           Queries

type OrderStub = (Operation, Asset, Scientific, Scientific)

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
    importer :: Connection -> (User, [OrderStub]) -> IO ()
    importer conn (user, orders) = do
      withTransaction conn $ do
        id_ <- addUser conn user
        mapM_
          (\x -> do
             order <- renderOrder x
             addOrder conn id_ order)
          orders
      pure ()
    renderOrder :: OrderStub -> IO Order
    renderOrder (op_, asset_, amount_, price_) = LimitOrder op_ asset_ amount_ price_ False <$> getCurrentTime
    users =
      [ (User "john" 100 100, [(Buy, USD, 10, 1.5), (Buy, EUR, 5, 1)])
      , (User "ripley" 200 200, [(Sell, EUR, 2, 1)])
      , (User "drummer" 300 300, [(Sell, EUR, 2, 1)])
      ]

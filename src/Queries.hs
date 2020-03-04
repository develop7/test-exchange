{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Queries where

import           Data.Scientific                  (Scientific)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, Only (..), execute, fromOnly, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Model

getBalances :: Connection -> String -> IO (Maybe [(Text, Scientific)])
getBalances conn handle = do
  row <- query conn "select handle, balance_eur, balance_usd from users where handle = ?" [handle]
  return $
    case row of
      [x] -> do
        let User {balanceEUR = be, balanceUSD = bu} = x
        Just [("EUR", be), ("USD", bu)]
      _ -> Nothing

createUsers :: Connection -> IO ()
createUsers conn = do
  _ <-
    execute
      conn
      [sql|
CREATE TABLE users
(
    id          SERIAL NOT NULL
        CONSTRAINT users_pk
            PRIMARY KEY,
    handle      TEXT   NOT NULL,
    balance_eur NUMERIC,
    balance_usd NUMERIC
);
      |]
      ()
  pure ()

createOrders :: Connection -> IO ()
createOrders conn = do
  _ <-
    execute
      conn
      [sql|
CREATE TABLE orders
(
    id          SERIAL                NOT NULL
        CONSTRAINT orders_pk
            PRIMARY KEY,
    user_id     INTEGER
        CONSTRAINT orders_users_id_fk
            REFERENCES users
            ON UPDATE CASCADE ON DELETE SET NULL,
    operation   TEXT,
    asset       TEXT,
    amount      NUMERIC,
    price       NUMERIC,
    is_complete BOOLEAN DEFAULT FALSE NOT NULL,
    created_at  TIMESTAMP WITH TIMEZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX orders__uid_complete
    ON orders (user_id, is_complete);
      |]
      ()
  pure ()

dropUsers :: Connection -> IO ()
dropUsers conn = do
  _ <- execute conn [sql|DROP TABLE IF EXISTS "users" |] ()
  return ()

dropOrders :: Connection -> IO ()
dropOrders conn = do
  _ <- execute conn [sql|DROP TABLE IF EXISTS "orders" |] ()
  return ()

addUser :: Connection -> User -> IO Int
addUser conn User {..} = do
  r :: [Only Int] <-
    query
      conn
      [sql|INSERT INTO "users" (handle, balance_eur, balance_usd) VALUES (?, ?, ?) RETURNING id|]
      (handle, balanceEUR, balanceUSD)
  return $ fromOnly . head $ r

addOrder :: Connection -> Int -> Order -> IO ()
addOrder conn user_id LimitOrder {..} = do
  _ <-
    execute
      conn
      [sql|INSERT INTO "orders" (user_id, operation, asset, amount, price, is_completed, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)|]
      (user_id, op, asset, amount, price, isCompleted, createdAt)
  pure ()

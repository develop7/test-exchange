{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Queries where

import           Data.Text                        (Text, unpack)
import           Database.PostgreSQL.Simple       (Connection, execute,
                                                   fromOnly, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Scientific (Scientific)

import           Model

getBalances :: Connection -> String -> IO (Maybe [(Text, Scientific)])
getBalances conn handle = do
  row <- query conn "select handle, balance_eur, balance_usd from users where handle = ?" [handle]
  return $
    case row of
      [] -> Nothing
      [x] -> do
        let User {balanceEUR = be, balanceUSD = bu} = x
        Just [("EUR", be), ("USD", bu)]

createUsers :: Connection -> IO ()
createUsers conn = do
  _ <-
    execute
      conn
      [sql| CREATE TABLE "users" (
    "handle"      TEXT PRIMARY KEY,
    "balance_eur" NUMERIC,
    "balance_usd" NUMERIC); |]
      ()
  pure ()
  
dropUsers :: Connection -> IO ()
dropUsers conn = do 
  _ <- execute conn [sql|DROP TABLE IF EXISTS "users" |] ()
  return ()

addUser :: Connection -> User -> IO ()
addUser conn User {..} = do
  r <-
    execute
      conn
      [sql|INSERT INTO "users" (handle, balance_eur, balance_usd) VALUES (?, ?, ?)|]
      (handle, balanceEUR, balanceUSD)
  pure ()

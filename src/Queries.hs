{-# LANGUAGE OverloadedStrings #-}

module Queries where

import           Data.Text                  (Text, unpack)
import           Database.PostgreSQL.Simple (Connection, fromOnly, query)

import           Model

getBalances :: Connection -> String -> IO (Maybe [(Text, Double)])
getBalances conn handle = do
--  [row] <- query conn "select handle, balance_eur, balance_usd from users where handle = ?" [handle]
  row <- query conn "select handle, balance_eur, balance_usd from (VALUES ('hey', 10, 20)) as t (handle, balance_eur, balance_usd) where handle = ?" [handle]
  return $ 
    case row of
      [] -> Nothing 
      [x] -> do
        let User {balanceEUR = be, balanceUSD = bu} = x  
        Just [("EUR", be), ("USD", bu)]

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Model                            (User (..))
import           Queries

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=cweb_exchange"
  
  dropUsers conn 
  createUsers conn
  
  mapM_ (addUser conn) users
  where
    users = [User "john" 100 100, User "ripley" 200 200, User "drummer" 300 300]

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple (connectPostgreSQL, defaultConnectInfo, ConnectInfo(..))
import           WebApp (runApp)

main :: IO ()
main = do
  conn <- connectPostgreSQL "postgres:///cweb_exchange"
  runApp conn

{-# LANGUAGE OverloadedStrings #-}

module WebApp
  ( runApp
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (Value (..), object, (.=))
import           Data.Text                  (Text, pack)
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (status404)
import           Network.Wai                (Application)
import qualified Web.Scotty                 as S

import           Queries

app' :: Connection -> S.ScottyM ()
app' conn = do
  S.get "/user/:handle/balance" $ do
    handle <- S.param "handle"
    balance <- liftIO $ getBalances conn handle
    case balance of
      Nothing -> do
        S.status status404
        S.text "404 Not Found"
      Just x -> S.json $ object $ map (\(currency, balance) -> currency .= Number balance) x

runApp :: Int -> Connection -> IO ()
runApp port conn = S.scotty port $ app' conn

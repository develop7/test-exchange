{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebApp
  ( runApp
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (Value (..), object, (.=))
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text, pack)
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (status404)
import           Network.Wai                (Application)
import qualified Web.Scotty                 as S

import           Model
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
  S.post "/order" $ do
    wat :: Order <- S.jsonData
    S.text "yep"

runApp :: Int -> Connection -> IO ()
runApp port conn = S.scotty port $ app' conn

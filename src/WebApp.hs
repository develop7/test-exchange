{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module WebApp
  ( runApp
  , app
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (Value (..), object, (.=))
import           Data.Text                  (Text, pack)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai                (Application)
import Network.HTTP.Types (status404)
import qualified Web.Scotty                 as S

import           Queries
import Data.Scientific (fromFloatDigits)

app' :: Connection -> S.ScottyM ()
app' conn = do
  S.get "/" $ do S.text "hello"
  S.get "/some-json" $ do S.json $ object ["foo" .= Number 23, "bar" .= Number 42]
  S.get "/user/:handle/balance" $ do
    handle <- S.param "handle"
    boo <- liftIO $ getBalances conn handle
    case boo of
      Nothing -> do 
        S.status status404
        S.text "404 Not Found"
      Just x -> S.json $ object $ map (\(currency, balance) -> currency .= Number (fromFloatDigits balance)) x

app :: Connection -> IO Application
app conn = S.scottyApp $ app' conn

runApp :: Connection -> IO ()
runApp conn = S.scotty 8080 $ app' conn

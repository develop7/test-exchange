{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebApp
  ( runApp
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             object, (.=))
import           Data.Scientific            (Scientific)
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Generics
import           Network.HTTP.Types         (status404)
import qualified Web.Scotty                 as S

import           Model
import           Queries

data OrderReq =
  OrderReq
    { username      :: String
    , op            :: Operation
    , asset         :: Asset
    , amount, price :: Scientific
    }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

app' :: Connection -> S.ScottyM ()
app' conn = do
  S.get "/user/:handle/balance" $ do
    handle <- S.param "handle"
    balance <- liftIO $ getBalances conn handle
    case balance of
      Nothing -> do
        S.status status404
        S.text "404 Not Found"
      Just x -> S.json $ object $ map (\(currency, blnc) -> currency .= Number blnc) x
  S.post "/order" $ do
    wat :: OrderReq <- S.jsonData
    S.text "yep"

runApp :: Int -> Connection -> IO ()
runApp port conn = S.scotty port $ app' conn

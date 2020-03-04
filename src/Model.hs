{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model where

import           Control.Monad                        (mzero)
import           Data.Aeson.Types                     (FromJSON, ToJSON)
import           Data.ByteString.Char8                (pack)
import           Data.Scientific                      (Scientific)
import           Data.Text                            (Text)
import           Data.Time.Clock                      (UTCTime)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow)
import           Database.PostgreSQL.Simple.ToField   (Action (..), ToField,
                                                       toField)
import           GHC.Generics

data Operation
  = Buy
  | Sell
  deriving (Generic, Show, Eq)

instance FromField Operation where
  fromField f mdata = do
    x <- fromField f mdata
    case x of
      "buy"  -> return Buy
      "sell" -> return Sell
      _      -> mzero

instance ToField Operation where
  toField Buy  = Escape $ pack "buy"
  toField Sell = Escape $ pack "sell"

instance FromJSON Operation

instance ToJSON Operation

data Asset
  = USD
  | EUR
  deriving (Generic, Show, Eq)

instance FromField Asset where
  fromField f mdata = do
    x <- fromField f mdata
    case x of
      "usd" -> return USD
      "eur" -> return EUR
      _     -> mzero

instance ToField Asset where
  toField USD = Escape $ pack "usd"
  toField EUR = Escape $ pack "eur"

instance FromJSON Asset

instance ToJSON Asset

data Order =
  LimitOrder
    { op            :: Operation
    , asset         :: Asset
    , amount, price :: Scientific
    , isCompleted   :: Bool
    , createdAt     :: UTCTime
    }
  deriving (Generic, Show, Eq, FromRow)

instance FromJSON Order

instance ToJSON Order

data User =
  User
    { handle                 :: Text
    , balanceEUR, balanceUSD :: Scientific
    }
  deriving (Generic, Show, Eq, FromRow)

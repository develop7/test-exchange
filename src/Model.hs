module Model where

import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Data.Scientific (Scientific)

data User = User {handle :: Text, balanceEUR, balanceUSD :: Scientific}

instance FromRow User where
  fromRow = User <$> field <*> field <*> field
  
--instance FromField User where
--  fromField
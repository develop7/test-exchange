module Model where

import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)

data User = User {handle :: Text, balanceEUR, balanceUSD :: Double}

instance FromRow User where
  fromRow = User <$> field <*> field <*> field
  
--instance FromField User where
--  fromField
{-# LANGUAGE DeriveGeneric     #-}

module Domain.ArmadaNonce where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data ArmadaNonce = ArmadaNonce
  { epochArmadaNonce :: Int
  , nonceArmadaNonce :: Text
  }
  deriving (Show, Generic)

instance ToJSON ArmadaNonce where
  toJSON p =
    object ["epoch" .= epochArmadaNonce p, "nonce" .= nonceArmadaNonce p]

instance FromJSON ArmadaNonce where
  parseJSON =
    withObject "ArmadaNonce" $ \v -> ArmadaNonce <$> v .: "epoch" <*> v .: "nonce"

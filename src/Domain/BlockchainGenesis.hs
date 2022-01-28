{-# LANGUAGE DeriveGeneric     #-}

module Domain.BlockchainGenesis where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data BlockchainGenesis = BlockchainGenesis
  { activeSlotsCoefficientBlockchainGenesis :: Float
  , updateQuorumBlockchainGenesis           :: Int
  , maxLovelaceSupplyBlockchainGenesis      :: Text
  , networkMagicBlockchainGenesis           :: Int
  , epochLengthBlockchainGenesis            :: Int
  , systemStartBlockchainGenesis            :: Int
  , slotsPerKesPeriodBlockchainGenesis      :: Int
  , slotLengthBlockchainGenesis             :: Int
  , maxKesEvolutionsBlockchainGenesis       :: Int
  , securityParamBlockchainGenesis          :: Int
  }
  deriving (Show, Generic)

instance ToJSON BlockchainGenesis where
  toJSON p = object
    [ "active_slots_coefficient" .= activeSlotsCoefficientBlockchainGenesis p
    , "update_quorum" .= updateQuorumBlockchainGenesis p
    , "max_lovelace_supply" .= maxLovelaceSupplyBlockchainGenesis p
    , "network_magic" .= networkMagicBlockchainGenesis p
    , "epoch_length" .= epochLengthBlockchainGenesis p
    , "system_start" .= systemStartBlockchainGenesis p
    , "slots_per_kes_period" .= slotsPerKesPeriodBlockchainGenesis p
    , "slot_length" .= slotLengthBlockchainGenesis p
    , "max_kes_evolutions" .= maxKesEvolutionsBlockchainGenesis p
    , "security_param" .= securityParamBlockchainGenesis p
    ]

instance FromJSON BlockchainGenesis where
  parseJSON = withObject "BlockchainGenesis" $ \v ->
    BlockchainGenesis
      <$> (v .: "active_slots_coefficient")
      <*> (v .: "update_quorum")
      <*> (v .: "max_lovelace_supply")
      <*> (v .: "network_magic")
      <*> (v .: "epoch_length")
      <*> (v .: "system_start")
      <*> (v .: "slots_per_kes_period")
      <*> (v .: "slot_length")
      <*> (v .: "max_kes_evolutions")
      <*> (v .: "security_param")

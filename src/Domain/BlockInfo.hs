{-# LANGUAGE DeriveGeneric     #-}

module Domain.BlockInfo where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data BlockInfo = BlockInfo
  { timeBlockInfo          :: Int
  , heightBlockInfo        :: Int
  , hashBlockInfo          :: Text
  , slotBlockInfo          :: Int
  , epochBlockInfo         :: Int
  , epochSlotBlockInfo     :: Int
  , slotLeaderBlockInfo    :: Text
  , sizeBlockInfo          :: Int
  , txCountBlockInfo       :: Int
  , outputBlockInfo        :: Text
  , feesBlockInfo          :: Text
  , blockVrfBlockInfo      :: Text
  , previousBlockBlockInfo :: Text
  , nextBlockBlockInfo     :: Text
  , confirmationsBlockInfo :: Int
  }
  deriving (Show, Generic)

instance ToJSON BlockInfo where
  toJSON p = object
    [ "time" .= timeBlockInfo p
    , "height" .= heightBlockInfo p
    , "hash" .= hashBlockInfo p
    , "slot" .= slotBlockInfo p
    , "epoch" .= epochBlockInfo p
    , "epoch_slot" .= epochSlotBlockInfo p
    , "slot_leader" .= slotLeaderBlockInfo p
    , "size" .= sizeBlockInfo p
    , "tx_count" .= txCountBlockInfo p
    , "output" .= outputBlockInfo p
    , "fees" .= feesBlockInfo p
    , "block_vrf" .= blockVrfBlockInfo p
    , "previous_block" .= previousBlockBlockInfo p
    , "next_block" .= nextBlockBlockInfo p
    , "confirmations" .= confirmationsBlockInfo p
    ]

instance FromJSON BlockInfo where
  parseJSON = withObject "BlockInfo" $ \v ->
    BlockInfo
      <$> (v .: "time")
      <*> (v .: "height")
      <*> (v .: "hash")
      <*> (v .: "slot")
      <*> (v .: "epoch")
      <*> (v .: "epoch_slot")
      <*> (v .: "slot_leader")
      <*> (v .: "size")
      <*> (v .: "tx_count")
      <*> (v .: "output")
      <*> (v .: "fees")
      <*> (v .: "block_vrf")
      <*> (v .: "previous_block")
      <*> (v .: "next_block")
      <*> (v .: "confirmations")

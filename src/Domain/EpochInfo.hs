{-# LANGUAGE DeriveGeneric     #-}

module Domain.EpochInfo where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Domain.ParseTool               ( parseInt64 )
import           GHC.Generics                   ( Generic )

data EpochInfo = EpochInfo
  { epochEpochInfo          :: Int
  , startTimeEpochInfo      :: Int
  , endTimeEpochInfo        :: Int
  , firstBlockTimeEpochInfo :: Int
  , lastBlockTimeEpochInfo  :: Int
  , blockCountEpochInfo     :: Int
  , txCountEpochInfo        :: Int
  , outputEpochInfo         :: Text
  , feesEpochInfo           :: Text
  , activeStakeEpochInfo    :: Int64
  }
  deriving (Show, Generic)

instance ToJSON EpochInfo where
  toJSON p = object
    [ "epoch" .= epochEpochInfo p
    , "start_time" .= startTimeEpochInfo p
    , "end_time" .= endTimeEpochInfo p
    , "first_block_time" .= firstBlockTimeEpochInfo p
    , "last_block_time" .= lastBlockTimeEpochInfo p
    , "block_count" .= blockCountEpochInfo p
    , "tx_count" .= txCountEpochInfo p
    , "output" .= outputEpochInfo p
    , "fees" .= feesEpochInfo p
    , "active_stake" .= activeStakeEpochInfo p
    ]

instance FromJSON EpochInfo where
  parseJSON = withObject "EpochInfo" $ \v ->
    EpochInfo
      <$> (v .: "epoch")
      <*> (v .: "start_time")
      <*> (v .: "end_time")
      <*> (v .: "first_block_time")
      <*> (v .: "last_block_time")
      <*> (v .: "block_count")
      <*> (v .: "tx_count")
      <*> (v .: "output")
      <*> (v .: "fees")
      <*> parseInt64 v "active_stake"

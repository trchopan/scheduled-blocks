{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.EpochSchedules
  ( EpochSchedules(..)
  , Schedule(..)
  , decodeTopLevel
  ) where

import           Data.Aeson
import           Data.Aeson.Types               ( emptyObject )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.HashMap.Strict            ( HashMap )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )

data EpochSchedules = EpochSchedules
  { epochEpochSchedules      :: Int
  , blockCountEpochSchedules :: Int
  , schedulesEpochSchedules  :: [Schedule]
  }
  deriving Show

data Schedule = Schedule
  { slotSchedule :: Int64
  , timeSchedule :: UTCTime
  }
  deriving Show

decodeTopLevel :: ByteString -> Maybe EpochSchedules
decodeTopLevel = decode

instance ToJSON EpochSchedules where
  toJSON (EpochSchedules epochEpochSchedules blockCountEpochSchedules schedulesEpochSchedules)
    = object
      [ "epoch" .= epochEpochSchedules
      , "blockCount" .= blockCountEpochSchedules
      , "schedules" .= schedulesEpochSchedules
      ]

instance FromJSON EpochSchedules where
  parseJSON (Object v) =
    EpochSchedules <$> v .: "epoch" <*> v .: "blockCount" <*> v .: "schedules"

instance ToJSON Schedule where
  toJSON (Schedule slotSchedule timeSchedule) =
    object ["slot" .= slotSchedule, "time" .= timeSchedule]

instance FromJSON Schedule where
  parseJSON (Object v) = Schedule <$> v .: "slot" <*> v .: "time"

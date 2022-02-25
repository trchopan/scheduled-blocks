{-# LANGUAGE DeriveGeneric     #-}

module Domain.CardanoGlobal where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                , addUTCTime
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           Domain.ParseTool               ( parseInt
                                                , parseUTCTime
                                                )
import           GHC.Generics                   ( Generic )


data Cardano = Cardano
  { syncBLocks   :: Int
  , lastBlock    :: Int
  , epochLast    :: Int
  , epochStarted :: UTCTime
  , endedBefore  :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Cardano where
  parseJSON = withObject "Cardano" $ \v ->
    Cardano
      <$> (v .: "sync_blocks")
      <*> parseInt v "last_block"
      <*> parseInt v "epoch_last"
      <*> parseUTCTime v "epoch_started"
      <*> parseUTCTime v "ended_before"

instance ToJSON Cardano where
  toJSON p = object
    [ "syncBLocks" .= syncBLocks p
    , "lastBlock" .= lastBlock p
    , "epochLast" .= epochLast p
    , "epochStarted" .= epochStarted p
    , "endedBefore" .= endedBefore p
    ]

data ArmdaNonce = ArmdaNonce
  { epoch :: Int
  , nonce :: String
  }

instance FromJSON ArmdaNonce where
  parseJSON = withObject "ArmdaNonce"
    $ \v -> ArmdaNonce <$> parseInt v "epoch" <*> v .: "nonce"

instance ToJSON ArmdaNonce where
  toJSON p = object ["epoch" .= epoch p, "nonce" .= nonce p]

secondsInDay :: NominalDiffTime
secondsInDay = 24 * 60 * 60

calculateNextEpoch :: Cardano -> Int -> UTCTime
calculateNextEpoch cardano n = addUTCTime
  ((fromRational . toRational) n * 5 * secondsInDay)
  (epochStarted cardano)

formatLocalTime :: UTCTime -> String
formatLocalTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

nextEpochs :: Cardano -> Int -> [String]
nextEpochs cardano n =
  [ (formatLocalTime . calculateNextEpoch cardano) i | i <- take n [1 ..] ]

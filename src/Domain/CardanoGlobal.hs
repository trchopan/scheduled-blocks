{-# LANGUAGE DeriveGeneric     #-}

module Domain.CardanoGlobal where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Time                      ( UTCTime )
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

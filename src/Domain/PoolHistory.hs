{-# LANGUAGE DeriveGeneric     #-}

module Domain.PoolHistory where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


data PoolHistory = PoolHistory
  { epochPoolHistory           :: Int
  , blocksPoolHistory          :: Int
  , activeStakePoolHistory     :: Text
  , activeSizePoolHistory      :: Float
  , delegatorsCountPoolHistory :: Int
  , rewardsPoolHistory         :: Text
  , feesPoolHistory            :: Text
  }
  deriving (Show, Generic)

instance ToJSON PoolHistory where
  toJSON p = object
    [ "epoch" .= epochPoolHistory p
    , "blocks" .= blocksPoolHistory p
    , "active_stake" .= activeStakePoolHistory p
    , "active_size" .= activeSizePoolHistory p
    , "delegators_count" .= delegatorsCountPoolHistory p
    , "rewards" .= rewardsPoolHistory p
    , "fees" .= feesPoolHistory p
    ]

instance FromJSON PoolHistory where
  parseJSON = withObject "PoolHistory" $ \v ->
    PoolHistory
      <$> (v .: "epoch")
      <*> (v .: "blocks")
      <*> (v .: "active_stake")
      <*> (v .: "active_size")
      <*> (v .: "delegators_count")
      <*> (v .: "rewards")
      <*> (v .: "fees")

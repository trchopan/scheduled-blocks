{-# LANGUAGE DeriveGeneric     #-}

module Domain.PoolInfo where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data PoolInfo = PoolInfo
  { poolIDPoolInfo         :: Text
  , hexPoolInfo            :: Text
  , vrfKeyPoolInfo         :: Text
  , blocksMintedPoolInfo   :: Int
  , blocksEpochPoolInfo    :: Int
  , liveStakePoolInfo      :: Text
  , liveSizePoolInfo       :: Float
  , liveSaturationPoolInfo :: Float
  , liveDelegatorsPoolInfo :: Int
  , activeStakePoolInfo    :: Text
  , activeSizePoolInfo     :: Float
  , declaredPledgePoolInfo :: Text
  , livePledgePoolInfo     :: Text
  , marginCostPoolInfo     :: Float
  , fixedCostPoolInfo      :: Text
  , rewardAccountPoolInfo  :: Text
  , ownersPoolInfo         :: [Text]
  , registrationPoolInfo   :: [Text]
  , retirementPoolInfo     :: [Maybe Text]
  }
  deriving (Show, Generic)

instance ToJSON PoolInfo where
  toJSON p = object
    [ "pool_id" .= poolIDPoolInfo p
    , "hex" .= hexPoolInfo p
    , "vrf_key" .= vrfKeyPoolInfo p
    , "blocks_minted" .= blocksMintedPoolInfo p
    , "blocks_epoch" .= blocksEpochPoolInfo p
    , "live_stake" .= liveStakePoolInfo p
    , "live_size" .= liveSizePoolInfo p
    , "live_saturation" .= liveSaturationPoolInfo p
    , "live_delegators" .= liveDelegatorsPoolInfo p
    , "active_stake" .= activeStakePoolInfo p
    , "active_size" .= activeSizePoolInfo p
    , "declared_pledge" .= declaredPledgePoolInfo p
    , "live_pledge" .= livePledgePoolInfo p
    , "margin_cost" .= marginCostPoolInfo p
    , "fixed_cost" .= fixedCostPoolInfo p
    , "reward_account" .= rewardAccountPoolInfo p
    , "owners" .= ownersPoolInfo p
    , "registration" .= registrationPoolInfo p
    , "retirement" .= retirementPoolInfo p
    ]

instance FromJSON PoolInfo where
  parseJSON = withObject "PoolInfo" $ \v ->
    PoolInfo
      <$> (v .: "pool_id")
      <*> (v .: "hex")
      <*> (v .: "vrf_key")
      <*> (v .: "blocks_minted")
      <*> (v .: "blocks_epoch")
      <*> (v .: "live_stake")
      <*> (v .: "live_size")
      <*> (v .: "live_saturation")
      <*> (v .: "live_delegators")
      <*> (v .: "active_stake")
      <*> (v .: "active_size")
      <*> (v .: "declared_pledge")
      <*> (v .: "live_pledge")
      <*> (v .: "margin_cost")
      <*> (v .: "fixed_cost")
      <*> (v .: "reward_account")
      <*> (v .: "owners")
      <*> (v .: "registration")
      <*> (v .: "retirement")

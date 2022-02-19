{-# LANGUAGE DeriveGeneric     #-}

module Domain.EpochParameter where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data EpochParameter = EpochParameter
  { epochEpochParameter                 :: Int
  , minFeeAEpochParameter               :: Int
  , minFeeBEpochParameter               :: Int
  , maxBlockSizeEpochParameter          :: Int
  , maxTxSizeEpochParameter             :: Int
  , maxBlockHeaderSizeEpochParameter    :: Int
  , keyDepositEpochParameter            :: Text
  , poolDepositEpochParameter           :: Text
  , eMaxEpochParameter                  :: Int
  , nOptEpochParameter                  :: Int
  , a0EpochParameter                    :: Float
  , rhoEpochParameter                   :: Float
  , tauEpochParameter                   :: Float
  , decentralisationParamEpochParameter :: Int
  , extraEntropyEpochParameter          :: Maybe Text
  , protocolMajorVerEpochParameter      :: Int
  , protocolMinorVerEpochParameter      :: Int
  , minUtxoEpochParameter               :: Text
  , minPoolCostEpochParameter           :: Text
  , nonceEpochParameter                 :: Text
  , priceMemEpochParameter              :: Float
  , priceStepEpochParameter             :: Float
  , maxTxExMemEpochParameter            :: Text
  , maxTxExStepsEpochParameter          :: Text
  , maxBlockExMemEpochParameter         :: Text
  , maxBlockExStepsEpochParameter       :: Text
  , maxValSizeEpochParameter            :: Text
  , collateralPercentEpochParameter     :: Int
  , maxCollateralInputsEpochParameter   :: Int
  , coinsPerUtxoWordEpochParameter      :: Text
  }
  deriving (Show, Generic)

instance ToJSON EpochParameter where
  toJSON p = object
    [ "epoch" .= epochEpochParameter p
    , "min_fee_a" .= minFeeAEpochParameter p
    , "min_fee_b" .= minFeeBEpochParameter p
    , "max_block_size" .= maxBlockSizeEpochParameter p
    , "max_tx_size" .= maxTxSizeEpochParameter p
    , "max_block_header_size" .= maxBlockHeaderSizeEpochParameter p
    , "key_deposit" .= keyDepositEpochParameter p
    , "pool_deposit" .= poolDepositEpochParameter p
    , "e_max" .= eMaxEpochParameter p
    , "n_opt" .= nOptEpochParameter p
    , "a0" .= a0EpochParameter p
    , "rho" .= rhoEpochParameter p
    , "tau" .= tauEpochParameter p
    , "decentralisation_param" .= decentralisationParamEpochParameter p
    , "extra_entropy" .= extraEntropyEpochParameter p
    , "protocol_major_ver" .= protocolMajorVerEpochParameter p
    , "protocol_minor_ver" .= protocolMinorVerEpochParameter p
    , "min_utxo" .= minUtxoEpochParameter p
    , "min_pool_cost" .= minPoolCostEpochParameter p
    , "nonce" .= nonceEpochParameter p
    , "price_mem" .= priceMemEpochParameter p
    , "price_step" .= priceStepEpochParameter p
    , "max_tx_ex_mem" .= maxTxExMemEpochParameter p
    , "max_tx_ex_steps" .= maxTxExStepsEpochParameter p
    , "max_block_ex_mem" .= maxBlockExMemEpochParameter p
    , "max_block_ex_steps" .= maxBlockExStepsEpochParameter p
    , "max_val_size" .= maxValSizeEpochParameter p
    , "collateral_percent" .= collateralPercentEpochParameter p
    , "max_collateral_inputs" .= maxCollateralInputsEpochParameter p
    , "coins_per_utxo_word" .= coinsPerUtxoWordEpochParameter p
    ]

instance FromJSON EpochParameter where
  parseJSON = withObject "EpochParameter" $ \v ->
    EpochParameter
      <$> (v .: "epoch")
      <*> (v .: "min_fee_a")
      <*> (v .: "min_fee_b")
      <*> (v .: "max_block_size")
      <*> (v .: "max_tx_size")
      <*> (v .: "max_block_header_size")
      <*> (v .: "key_deposit")
      <*> (v .: "pool_deposit")
      <*> (v .: "e_max")
      <*> (v .: "n_opt")
      <*> (v .: "a0")
      <*> (v .: "rho")
      <*> (v .: "tau")
      <*> (v .: "decentralisation_param")
      <*> (v .: "extra_entropy")
      <*> (v .: "protocol_major_ver")
      <*> (v .: "protocol_minor_ver")
      <*> (v .: "min_utxo")
      <*> (v .: "min_pool_cost")
      <*> (v .: "nonce")
      <*> (v .: "price_mem")
      <*> (v .: "price_step")
      <*> (v .: "max_tx_ex_mem")
      <*> (v .: "max_tx_ex_steps")
      <*> (v .: "max_block_ex_mem")
      <*> (v .: "max_block_ex_steps")
      <*> (v .: "max_val_size")
      <*> (v .: "collateral_percent")
      <*> (v .: "max_collateral_inputs")
      <*> (v .: "coins_per_utxo_word")

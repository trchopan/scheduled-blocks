{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSample where

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



data TestSample = TestSample
  { activeSlotCoeffTestSample :: Float
  , sigmaTestSample           :: Float
  , poolTickerTestSample      :: Text
  , nonceTestSample           :: Text
  , vrfSkeyTestSample         :: Text
  , seedLBytesTestSample      :: Text
  , certNatMaxTestSample      :: Text
  , slotsTestSample           :: [Slot]
  }
  deriving (Show, Generic)

data Slot = Slot
  { slotToSeedBytesSlot :: Text
  , mkSeedSlot          :: Text
  , proofHashRawSlot    :: Text
  , certNatSlot         :: Text
  , denominatorSlot     :: Float
  , qSlot               :: Float
  , cSlot               :: Float
  , sigmaOfFSlot        :: Float
  , slotSlot            :: Int64
  }
  deriving (Show, Generic)

instance ToJSON TestSample where
  toJSON p = object
    [ "activeSlotCoeff" .= activeSlotCoeffTestSample p
    , "sigma" .= sigmaTestSample p
    , "poolTicker" .= poolTickerTestSample p
    , "nonce" .= nonceTestSample p
    , "vrfSkey" .= vrfSkeyTestSample p
    , "seedLBytes" .= seedLBytesTestSample p
    , "certNatMax" .= certNatMaxTestSample p
    , "slots" .= slotsTestSample p
    ]
instance FromJSON TestSample where
  parseJSON = withObject "TestSample" $ \v ->
    TestSample
      <$> (v .: "activeSlotCoeff")
      <*> (v .: "sigma")
      <*> (v .: "poolTicker")
      <*> (v .: "nonce")
      <*> (v .: "vrfSkey")
      <*> (v .: "seedLBytes")
      <*> (v .: "certNatMax")
      <*> (v .: "slots")

instance ToJSON Slot where
  toJSON p = object
    [ "slotToSeedBytes" .= slotToSeedBytesSlot p
    , "mkSeed" .= mkSeedSlot p
    , "proofHashRaw" .= proofHashRawSlot p
    , "certNat" .= certNatSlot p
    , "denominator" .= denominatorSlot p
    , "q" .= qSlot p
    , "c" .= cSlot p
    , "sigmaOfF" .= sigmaOfFSlot p
    , "slot" .= slotSlot p
    ]

instance FromJSON Slot where
  parseJSON = withObject "Slot" $ \v ->
    Slot
      <$> (v .: "slotToSeedBytes")
      <*> (v .: "mkSeed")
      <*> (v .: "proofHashRaw")
      <*> (v .: "certNat")
      <*> (v .: "denominator")
      <*> (v .: "q")
      <*> (v .: "c")
      <*> (v .: "sigmaOfF")
      <*> parseInt64 v "slot"

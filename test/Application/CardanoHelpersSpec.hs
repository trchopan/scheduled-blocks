module Application.CardanoHelpersSpec
  ( spec
  ) where

import           Application.CardanoHelpers     ( hashBlake2b
                                                , mkSeed
                                                , seedLBytes
                                                , slotToSeedBytes
                                                , slotToTime
                                                )
import           Application.CommonHelpers      ( decodeBS
                                                , textToBS
                                                )
import           Data.ByteString.Base16         ( encode )
import           Data.ByteString.UTF8           ( fromString )
import qualified Data.Text                     as T
import           Data.Time                      ( defaultTimeLocale
                                                , parseTimeOrError
                                                )
import           GHC.IO                         ( bracket )
import           LoadSample                     ( readTestSampleOrError )
import           Test.Hspec                     ( Spec
                                                , SpecWith
                                                , around
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           TestSample                     ( Slot
                                                  ( mkSeedSlot
                                                  , slotSlot
                                                  , slotToSeedBytesSlot
                                                  )
                                                , TestSample
                                                  ( nonceTestSample
                                                  , seedLBytesTestSample
                                                  , slotsTestSample
                                                  )
                                                )

withLoadedSamples :: (TestSample -> IO ()) -> IO ()
withLoadedSamples = bracket readTestSampleOrError (\_ -> return ())

spec :: Spec
spec = do
  around withLoadedSamples $ do
    mkSeedSpec

mkSeedSpec :: SpecWith TestSample
mkSeedSpec =
  describe
      "handle the making seed bytes and calculations related to the Cardano Stake Pools"
    $ do
        it "has correct seedLBytes" $ \sample -> do
          let expected = T.unpack (seedLBytesTestSample sample)
          show seedLBytes `shouldBe` expected

        it "mkSeed (xor) the seedLBytes and the hashedSeedBytes" $ \sample -> do
          let withSlotTest :: Slot -> IO ()
              withSlotTest s = do
                let slotNumber = slotSlot s
                    decodedNonce = decodeBS $ textToBS $ nonceTestSample sample
                    seedBytes = slotToSeedBytes slotNumber decodedNonce
                    hashedSeedBytes = hashBlake2b seedBytes
                    expectedSlotSeedBytes = T.unpack (slotToSeedBytesSlot s)

                    seed = mkSeed seedLBytes hashedSeedBytes
                    encodedSeedResult = encode seed
                    expectedSeedBytes = (fromString . T.unpack) (mkSeedSlot s)

                show hashedSeedBytes `shouldBe` expectedSlotSeedBytes
                encodedSeedResult `shouldBe` expectedSeedBytes

          mapM_ withSlotTest (slotsTestSample sample)

        it "calculate localtime from slot" $ \_ -> do
          let timeFormat = "%F %T %z"
              parseTime  = parseTimeOrError True defaultTimeLocale timeFormat
          slotToTime 53309568 `shouldBe` parseTime "2022-02-14 21:57:39 +0000"

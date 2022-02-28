module Repository.Cardano.Crypto.VRF.PraosSpec
  ( spec
  ) where

import           Application.CardanoHelpers     ( certNatMax )
import           Application.CommonHelpers      ( bytestringToNatural
                                                , decodeBS
                                                )
import           Data.ByteString.Base16         ( encode )
import           Data.ByteString.UTF8           ( fromString
                                                , toString
                                                )
import qualified Data.Text                     as T
import           GHC.Base                       ( eqFloat )
import           GHC.IO                         ( bracket )
import           LoadSample                     ( readTestSampleOrError )
import           Repository.Cardano.Crypto.VRF.Praos
                                                ( outputBytes
                                                , outputFromProof
                                                , prove
                                                , skFromBytes
                                                )
import           Repository.KeyFile             ( loadVrfSkey
                                                , poolVrfSkey
                                                )
import           Test.Hspec                     ( Spec
                                                , SpecWith
                                                , around
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           TestSample                     ( Slot
                                                  ( certNatSlot
                                                  , mkSeedSlot
                                                  , proofHashRawSlot
                                                  , qSlot
                                                  , sigmaOfFSlot
                                                  )
                                                , TestSample
                                                  ( activeSlotCoeffTestSample
                                                  , certNatMaxTestSample
                                                  , sigmaTestSample
                                                  , slotsTestSample
                                                  , vrfSkeyTestSample
                                                  )
                                                )

withLoadedSamples :: (TestSample -> IO ()) -> IO ()
withLoadedSamples = bracket readTestSampleOrError (\_ -> return ())

spec :: Spec
spec = do
  around withLoadedSamples $ do
    praosSpec

praosSpec :: SpecWith TestSample
praosSpec = do
  describe
      "use the libsodium library to calculate the vrf proof using the provided pool signing key"
    $ do
        it "has correct certMax" $ \sample -> do
          let expected = T.unpack $ certNatMaxTestSample sample
          show certNatMax `shouldBe` expected

        it "can load vrfSKey" $ \sample -> do
          let vrfKeyStr = T.unpack (vrfSkeyTestSample sample)
          vrfSignKey <- loadVrfSkey "./test/vrf.skey"
          poolVrfSkey vrfSignKey `shouldBe` vrfKeyStr

        it "can calculate proof and sigmaOfF" $ \sample -> do
          let
            vrfKeyStr   = T.unpack (vrfSkeyTestSample sample)
            vrfKeyBytes = (decodeBS . fromString) vrfKeyStr
            vrfKey      = skFromBytes vrfKeyBytes
            withSlotTest :: Slot -> IO ()
            withSlotTest s = do
              let
                seed       = decodeBS $ fromString $ T.unpack $ mkSeedSlot s
                maybeProof = prove vrfKey seed
                proofHash =
                  maybe "" (maybe "" outputBytes . outputFromProof) maybeProof
                expectedProofHash = T.unpack $ proofHashRawSlot s

              let
                certNat         = bytestringToNatural proofHash
                activeSlotCoeff = activeSlotCoeffTestSample sample
                sigma           = sigmaTestSample sample
                denominator     = certNatMax - certNat
                q = realToFrac (toRational certNatMax / toRational denominator)
                c               = log (1.0 - activeSlotCoeff)
                sigmaOfF        = exp (-sigma * c)

              (toString . encode) proofHash `shouldBe` expectedProofHash
              certNat `shouldBe` read (T.unpack $ certNatSlot s)

              q `eqFloat` qSlot s `shouldBe` True

              sigmaOfF `eqFloat` sigmaOfFSlot s `shouldBe` True

          mapM_ withSlotTest (slotsTestSample sample)

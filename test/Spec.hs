{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString                ( pack )
import           Data.ByteString.Base16         ( encode )
import qualified Data.ByteString.Lazy          as LBS
import           Data.ByteString.UTF8           ( fromString
                                                , toString
                                                )
import qualified Data.Text                     as T
import           Repository.Cardano.Crypto.VRF.Praos
                                                ( outputBytes
                                                , outputFromProof
                                                , prove
                                                , skFromBytes
                                                )
import           Repository.Crypto              ( bytestringToNatural
                                                , certNatMax
                                                , decodeBS
                                                , hashBlake2b
                                                , mkSeed
                                                , seedLBytes
                                                , slotToSeedBytes
                                                , textToBS
                                                )
import           Repository.KeyFile             ( loadVrfSkey
                                                , poolVrfSkey
                                                )
import           Test.Hspec                     ( Arg
                                                , Expectation
                                                , SpecWith
                                                , describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )
import           TestSample                     ( Slot
                                                  ( certNatSlot
                                                  , mkSeedSlot
                                                  , proofHashRawSlot
                                                  , sigmaOfFSlot
                                                  , slotSlot
                                                  , slotToSeedBytesSlot
                                                  )
                                                , TestSample
                                                  ( activeSlotCoeffTestSample
                                                  , certNatMaxTestSample
                                                  , nonceTestSample
                                                  , seedLBytesTestSample
                                                  , sigmaTestSample
                                                  , slotsTestSample
                                                  , vrfSkeyTestSample
                                                  )
                                                )


readTestSample :: IO (Either String TestSample)
readTestSample = eitherDecode <$> LBS.readFile "./test/test-example.json"


main :: IO ()
main = readTestSample >>= \case
  Left  err    -> print err
  Right sample -> do
    hspec $ mkSeedSpec sample
    hspec $ praosSpec sample


mkSeedSpec :: TestSample -> SpecWith ()
mkSeedSpec sample = describe "Repository.Crypto" $ do
  it "has correct seedLBytes" $ do
    let expected = T.unpack (seedLBytesTestSample sample)
    show seedLBytes `shouldBe` expected

  let withSlotTest :: Slot -> SpecWith (Arg Expectation)
      withSlotTest s = do
        let slotNumber            = slotSlot s
            decodedNonce          = decodeBS $ textToBS $ nonceTestSample sample
            seedBytes             = slotToSeedBytes slotNumber decodedNonce
            hashedSeedBytes       = hashBlake2b seedBytes
            expectedSlotSeedBytes = T.unpack (slotToSeedBytesSlot s)
        it "concat the slot and seed bytes then parse correctly" $ do
          show hashedSeedBytes `shouldBe` expectedSlotSeedBytes

        let mkSeedResult      = mkSeed seedLBytes hashedSeedBytes
            encodedSeedResult = (encode . pack) mkSeedResult
            expectedSeedBytes = (fromString . T.unpack) (mkSeedSlot s)
        it "mkSeed (xor) the seedLBytes and the hashedSeedBytes" $ do
          encodedSeedResult `shouldBe` expectedSeedBytes

  mapM_ withSlotTest (slotsTestSample sample)


praosSpec :: TestSample -> SpecWith ()
praosSpec sample = describe "Repository.Cardano.Crypto.VRF.Praos" $ do
  it "has correct certMax" $ do
    let expected = T.unpack $ certNatMaxTestSample sample
    show certNatMax `shouldBe` expected

  let vrfKeyStr = T.unpack (vrfSkeyTestSample sample)
  it "can load vrfSKey" $ do
    vrfSignKey <- loadVrfSkey "./test/vrf.skey"
    poolVrfSkey vrfSignKey `shouldBe` vrfKeyStr

  let
    vrfSignKeyStr = T.unpack (vrfSkeyTestSample sample)
    vrfKeyBytes   = (decodeBS . fromString) vrfKeyStr
    vrfKey        = skFromBytes vrfKeyBytes
    withSlotTest :: Slot -> SpecWith (Arg Expectation)
    withSlotTest s = do
      let
        mkSeed     = decodeBS $ fromString $ T.unpack $ mkSeedSlot s
        maybeProof = prove vrfKey mkSeed
        proofHash =
          maybe "" (maybe "" outputBytes . outputFromProof) maybeProof
        expectedProofHash = T.unpack $ proofHashRawSlot s
      it "can make vrf proof" $ do
        (toString . encode) proofHash `shouldBe` expectedProofHash

      let certNat         = bytestringToNatural proofHash
          activeSlotCoeff = activeSlotCoeffTestSample sample
          sigma           = sigmaTestSample sample
          denominator     = certNatMax - certNat
          q = realToFrac (toRational certNatMax / toRational denominator)
          c               = log (1.0 - activeSlotCoeff)
          sigmaOfF        = exp (-sigma * c)

          expectSigmaOfF  = sigmaOfFSlot s

      it "can calculate certNat" $ do
        certNat `shouldBe` read (T.unpack $ certNatSlot s)

      it "can calculate sigmaOfF" $ do
        (sigmaOfF - expectSigmaOfF) < 0.00001 `shouldBe` True


  mapM_ withSlotTest (slotsTestSample sample)

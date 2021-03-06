{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Application.CardanoHelpers where

import           Application.CommonHelpers      ( bytestringToNatural
                                                , utcFromSecs
                                                )
import           Crypto.Hash                    ( Blake2b_256()
                                                , Digest
                                                , hash
                                                )
import qualified Data.Binary                   as Binary
import           Data.Bits                      ( Bits(xor) )
import qualified Data.ByteArray                as BA
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Int                       ( Int64 )
import           Data.Time                      ( UTCTime )
import           Repository.Cardano.Crypto.VRF.Praos
                                                ( outputBytes
                                                , outputFromProof
                                                , prove
                                                , skFromBytes
                                                )


certNatMax :: Integer
certNatMax = 2 ^ 512

mkSigmaOfF :: Float -> Float -> Float
mkSigmaOfF activeSlotCoeff sigma = exp (-sigma * c)
  where c = log (1.0 - activeSlotCoeff)

hashBlake2b :: ByteString -> Digest Blake2b_256
hashBlake2b = hash

seedLBytes :: Digest Blake2b_256
seedLBytes = hashBlake2b neutral
  where neutral = BA.pack [0, 0, 0, 0, 0, 0, 0, 1] :: ByteString

slotToSeedBytes :: Int64 -> ByteString -> ByteString
slotToSeedBytes slot = BS.append encodedSlot
  where encodedSlot = LBS.toStrict $ Binary.encode slot

-- For every seedLBytes xor it with the slotSeedBytes
mkSeed :: Digest Blake2b_256 -> Digest Blake2b_256 -> ByteString
mkSeed seedLBytes slotToSeedBytes = BS.pack
  $ zipWith xor arrSeedLBytes arrSlotToSeedBytes
 where
  arrSeedLBytes      = BA.unpack seedLBytes
  arrSlotToSeedBytes = BA.unpack slotToSeedBytes

slotToTime :: Int64 -> UTCTime
slotToTime slot = utcFromSecs (slot + 1591566291)

isSlotLeader :: Float -> ByteString -> ByteString -> Int64 -> Bool
isSlotLeader sigmaOfF nonce vrfSkey slotNumber = q < sigmaOfF
 where
  seedBytes       = slotToSeedBytes slotNumber nonce
  hashedSeedBytes = hashBlake2b seedBytes
  seed            = mkSeed seedLBytes hashedSeedBytes
  maybeProof      = prove (skFromBytes vrfSkey) seed
  proofHash       = maybe "" (maybe "" outputBytes . outputFromProof) maybeProof
  certNat         = bytestringToNatural proofHash
  denominator     = certNatMax - certNat
  q               = realToFrac (toRational certNatMax / toRational denominator)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Infrastructure.Crypto where

import           Crypto.Hash                    ( Blake2b_256()
                                                , Digest
                                                , hash
                                                )
import qualified Data.Binary                   as Binary
import           Data.Bits                      ( Bits(xor) )
import qualified Data.ByteArray                as BA
import qualified Data.ByteArray                as B
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Int                       ( Int64 )
import           Data.Word                      ( Word8 )


currentNonce :: LBS.ByteString
currentNonce =
  "97be25ab0a46a6537faaf32f882de17611c897f32c7eeef12f53a176b225e461"

sampleSlot :: Int64
sampleSlot = 53308800

hashBlake2b :: BS.ByteString -> Digest Blake2b_256
hashBlake2b = hash

seedLBytes :: Digest Blake2b_256
seedLBytes = hashBlake2b neutral
  where neutral = BA.pack [0, 0, 0, 0, 0, 0, 0, 1] :: ByteString

slotToSeedBytes :: Int64 -> ByteString -> Digest Blake2b_256
slotToSeedBytes slot nonce = hashBlake2b $ BS.append encodedSlot nonce
  where encodedSlot = LBS.toStrict $ Binary.encode slot

-- For every seedLBytes xor it with the slotSeedBytes
mkSeed :: Digest Blake2b_256 -> Digest Blake2b_256 -> [Word8]
mkSeed seedLBytes slotToSeedBytes = zipWith xor
                                            arrSeedLBytes
                                            arrSlotToSeedBytes
 where
  arrSeedLBytes      = B.unpack seedLBytes
  arrSlotToSeedBytes = B.unpack slotToSeedBytes

module Repository.Crypto where

import           Crypto.Hash                    ( Blake2b_256()
                                                , Digest
                                                , hash
                                                )
import qualified Data.Binary                   as Binary
import           Data.Bits                      ( Bits(xor) )
import qualified Data.ByteArray                as BA
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.ByteString.Base16         ( decode
                                                , encode
                                                )
import qualified Data.ByteString.Lazy          as LSB
import           Data.ByteString.UTF8           ( fromString
                                                , toString
                                                )
import           Data.Int                       ( Int64 )
import qualified Data.Text                     as T
import           Data.Word                      ( Word8 )
import           GHC.IO                         ( unsafePerformIO )
import           Numeric                        ( readHex )

certNatMax :: Integer
certNatMax = 2 ^ 512

textToBS :: T.Text -> ByteString
textToBS = fromString . T.unpack

decodeBS :: ByteString -> ByteString
decodeBS bs = unsafePerformIO $ do
  case decode bs of
    Left  err -> error "cannot decode"
    Right x   -> return x

hashBlake2b :: ByteString -> Digest Blake2b_256
hashBlake2b = hash

seedLBytes :: Digest Blake2b_256
seedLBytes = hashBlake2b neutral
  where neutral = BA.pack [0, 0, 0, 0, 0, 0, 0, 1] :: ByteString


slotToSeedBytes :: Int64 -> ByteString -> ByteString
slotToSeedBytes slot = BS.append encodedSlot
  where encodedSlot = LSB.toStrict $ Binary.encode slot

-- For every seedLBytes xor it with the slotSeedBytes
mkSeed :: Digest Blake2b_256 -> Digest Blake2b_256 -> [Word8]
mkSeed seedLBytes slotToSeedBytes = zipWith xor
                                            arrSeedLBytes
                                            arrSlotToSeedBytes
 where
  arrSeedLBytes      = BA.unpack seedLBytes
  arrSlotToSeedBytes = BA.unpack slotToSeedBytes

bytestringToNatural :: ByteString -> Integer
bytestringToNatural = fromRead . readHex . toString . encode
 where
  fromRead :: [(Integer, String)] -> Integer
  fromRead []       = 0
  fromRead (x : xs) = fst x

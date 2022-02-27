module Application.Cardano where

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
import           Data.Text.Format.Numbers       ( PrettyCfg(PrettyCfg)
                                                , prettyF
                                                )
import           Data.Time                      ( Day
                                                , NominalDiffTime
                                                , UTCTime(UTCTime)
                                                , addUTCTime
                                                , defaultTimeLocale
                                                , formatTime
                                                , fromGregorian
                                                , getZonedTime
                                                , hoursToTimeZone
                                                , secondsToDiffTime
                                                , utcToLocalTime
                                                )
import           Domain.CardanoGlobal           ( Cardano(epochStarted)
                                                , epochStarted
                                                )
import           GHC.IO                         ( unsafePerformIO )
import           Numeric                        ( readHex )


prettyInt :: Integer -> T.Text
prettyInt = prettyF (PrettyCfg 0 (Just ',') '.') . toRational . toInteger

certNatMax :: Integer
certNatMax = 2 ^ 512

mkSigmaOfF :: Float -> Float -> Float
mkSigmaOfF activeSlotCoeff sigma = exp (-sigma * c)
  where c = log (1.0 - activeSlotCoeff)

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
mkSeed :: Digest Blake2b_256 -> Digest Blake2b_256 -> ByteString
mkSeed seedLBytes slotToSeedBytes = BS.pack
  $ zipWith xor arrSeedLBytes arrSlotToSeedBytes
 where
  arrSeedLBytes      = BA.unpack seedLBytes
  arrSlotToSeedBytes = BA.unpack slotToSeedBytes

bytestringToNatural :: ByteString -> Integer
bytestringToNatural = fromRead . readHex . toString . encode
 where
  fromRead :: [(Integer, String)] -> Integer
  fromRead []       = 0
  fromRead (x : xs) = fst x

jan_1_1970_day :: Day
jan_1_1970_day = fromGregorian 1970 1 1 :: Day

jan_1_1970_time :: UTCTime
jan_1_1970_time = UTCTime jan_1_1970_day (secondsToDiffTime 0)

utcFromSecs :: Integral a => a -> UTCTime
utcFromSecs secs = addUTCTime (fromIntegral secs) jan_1_1970_time

secondsInDay :: NominalDiffTime
secondsInDay = 24 * 60 * 60

calculateNextEpoch :: UTCTime -> Int -> UTCTime
calculateNextEpoch epochStarted n = addUTCTime
  ((fromRational . toRational) n * 5 * secondsInDay) -- 1 epoch = 5 days
  epochStarted

formatLocalTime :: UTCTime -> String
formatLocalTime t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" t'
  where t' = utcToLocalTime (hoursToTimeZone 7) t

nextEpochs :: Cardano -> Int -> [String]
nextEpochs cardano n =
  [ (formatLocalTime . calculateNextEpoch (epochStarted cardano)) i
  | i <- take n [1 ..]
  ]

slotToTime :: Int64 -> UTCTime
slotToTime slot = utcFromSecs (slot + 1591566291)

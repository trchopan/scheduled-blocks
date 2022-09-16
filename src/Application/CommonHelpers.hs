module Application.CommonHelpers where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Base16        as B16
import           Data.ByteString.UTF8           ( fromString
                                                , toString
                                                )
import           Data.Int                       ( Int64 )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Text.Format.Numbers       ( PrettyCfg(PrettyCfg)
                                                , prettyF
                                                )
import           Data.Time                      ( Day
                                                , NominalDiffTime
                                                , TimeZone
                                                , UTCTime(UTCTime)
                                                , addUTCTime
                                                , defaultTimeLocale
                                                , formatTime
                                                , fromGregorian
                                                , secondsToDiffTime
                                                , utcToZonedTime
                                                )
import           Domain.ArmadaNonce             ( ArmadaNonce )
import           Domain.BlockInfo               ( BlockInfo )
import           Domain.BlockchainGenesis       ( BlockchainGenesis )
import           Domain.EpochInfo               ( EpochInfo )
import           Domain.EpochParameter          ( EpochParameter(EpochParameter)
                                                )
import           Domain.PoolHistory             ( PoolHistory )
import           Domain.PoolInfo                ( PoolInfo )
import           Numeric                        ( readHex )
import           Repository.Api                 ( getBlockchainGenesis
                                                , getEpochInfo
                                                , getEpochParam
                                                , getFirstShellyBlock
                                                , getLatestEpochParam
                                                , getNextNonce
                                                , getPoolHistory
                                                , getPoolInfo
                                                , requestAndDecode
                                                )
import           System.ProgressBar             ( OnComplete(Clear)
                                                , Progress(Progress)
                                                , ProgressBar
                                                , ProgressBarWidth
                                                  ( ConstantWidth
                                                  )
                                                , Style
                                                  ( Style
                                                  , styleClose
                                                  , styleCurrent
                                                  , styleDone
                                                  , styleEscapeClose
                                                  , styleEscapeCurrent
                                                  , styleEscapeDone
                                                  , styleEscapeOpen
                                                  , styleEscapePostfix
                                                  , styleEscapePrefix
                                                  , styleEscapeTodo
                                                  , styleOnComplete
                                                  , styleOpen
                                                  , stylePostfix
                                                  , stylePrefix
                                                  , styleTodo
                                                  , styleWidth
                                                  )
                                                , msg
                                                , newProgressBar
                                                , percentage
                                                )
import           Text.Printf                    ( printf )

withStatusMessage :: String -> IO b -> IO b
withStatusMessage message mOp = do
  putStr message
  result <- mOp
  putStr "done\n"
  return result

prettyInt :: Integer -> T.Text
prettyInt = prettyF (PrettyCfg 0 (Just ',') '.') . toRational . toInteger

textToBS :: T.Text -> ByteString
textToBS = fromString . T.unpack

decodeB16OrError :: ByteString -> ByteString
decodeB16OrError bs = case B16.decode bs of
  Left  err -> error "cannot decode"
  Right x   -> x

bytestringToNatural :: ByteString -> Integer
bytestringToNatural = fromRead . readHex . toString . B16.encode
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

timeToString :: UTCTime -> TimeZone -> String
timeToString t tz = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" t'
  where t' = utcToZonedTime tz t

percentageProcessBar :: IO (ProgressBar ())
percentageProcessBar = do
  let pbStyle = Style { styleOpen          = "["
                      , styleClose         = "]"
                      , styleDone          = '='
                      , styleCurrent       = '>'
                      , styleTodo          = '.'
                      , stylePrefix        = msg "Working"
                      , stylePostfix       = percentage
                      , styleWidth         = ConstantWidth 40
                      , styleEscapeOpen    = const ""
                      , styleEscapeClose   = const ""
                      , styleEscapeDone    = const ""
                      , styleEscapeCurrent = const ""
                      , styleEscapeTodo    = const ""
                      , styleEscapePrefix  = const ""
                      , styleEscapePostfix = const ""
                      , styleOnComplete    = Clear
                      }
  newProgressBar pbStyle 10 (Progress 0 100 ())

armadaNonceRequest :: IO ArmadaNonce
armadaNonceRequest = withStatusMessage "Checking next network epoch..."
  $ requestAndDecode getNextNonce

latestEpochRequest :: String -> IO EpochParameter
latestEpochRequest blockFrostApi =
  withStatusMessage "Checking current network epoch..."
    $ requestAndDecode
    $ getLatestEpochParam blockFrostApi

epochParamRequest :: String -> Int -> IO EpochParameter
epochParamRequest blockFrostApi epoch =
  withStatusMessage "Checking epoch parameters..."
    $ requestAndDecode
    $ getEpochParam blockFrostApi epoch

poolInfoRequest :: String -> String -> IO PoolInfo
poolInfoRequest blockFrostApi poolId =
  withStatusMessage "Checking Current Pool Sigma..."
    $ requestAndDecode
    $ getPoolInfo blockFrostApi poolId

poolHistoryRequest :: String -> String -> IO [PoolHistory]
poolHistoryRequest blockFrostApi poolId =
  withStatusMessage "Checking Pool Sigma from Pool History..."
  $ requestAndDecode
  $ getPoolHistory blockFrostApi poolId :: IO [PoolHistory]


epochInfoRequest :: String -> Int -> IO EpochInfo
epochInfoRequest blockFrostApi epoch =
  withStatusMessage "Checking epoch info for active stake..."
    $ requestAndDecode
    $ getEpochInfo blockFrostApi epoch

genesisRequest :: String -> IO BlockchainGenesis
genesisRequest blockFrostApi =
  withStatusMessage "Checking network genenis..."
    $ requestAndDecode
    $ getBlockchainGenesis blockFrostApi

firstShellyBlockRequest :: String -> IO BlockInfo
firstShellyBlockRequest blockFrostApi =
  withStatusMessage "Get first Shelly block"
    $ requestAndDecode
    $ getFirstShellyBlock blockFrostApi


data PrintInfomation = PrintInfomation
  { nonce            :: Text
  , activeSlotCoeff  :: Float
  , epochLength      :: Int64
  , slotLength       :: Int
  , firstSlotOfEpoch :: Int64
  , activeStake      :: Integer
  , poolActiveStake  :: Integer
  , poolSigma        :: Float
  }

printInformation :: PrintInfomation -> IO ()
printInformation (PrintInfomation nonce activeSlotCoeff epochLength slotLength firstSlotOfEpoch activeStake poolActiveStake poolSigma)
  = do
    printf "Nonce: %s\n"                     nonce
    printf "Active Slot Coefficient: %.3f\n" activeSlotCoeff
    printf "Epoch Length: %d\n"              epochLength
    printf "Slot Length: %d\n"               slotLength
    printf "First Slot of Epoch: %d\n"       firstSlotOfEpoch
    printf "Last Slot of Epoch: %d\n"        (firstSlotOfEpoch + epochLength)
    printf "Active Stake: %s\n"              (prettyInt activeStake)
    printf "Pool Active Stake: %s\n"         (prettyInt poolActiveStake)
    printf "Pool Sigma: %.9f\n"              poolSigma

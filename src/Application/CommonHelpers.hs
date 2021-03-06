module Application.CommonHelpers where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Base16        as B16
import           Data.ByteString.UTF8           ( fromString
                                                , toString
                                                )
import qualified Data.Text                     as T
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
import           Numeric                        ( readHex )
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

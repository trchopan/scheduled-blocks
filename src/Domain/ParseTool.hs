module Domain.ParseTool
  ( parseInt
  , parseFloat
  , parseUTCTime
  ) where

import           Data.Aeson                     ( (.:)
                                                , Key
                                                , Object
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Foldable                  ( asum )
import           Data.Time                      ( UTCTime )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Debug.Trace                    ( trace )
import           Text.Read                      ( readMaybe )


parseInt :: Object -> Key -> Parser Int
parseInt v field = asum
  [ v .: field
  , do
    s <- v .: field
    case readMaybe s :: Maybe Int of
      Nothing -> trace ("failed s=" ++ s) $ fail "not a number"
      Just x  -> return x
  ]

parseFloat :: Object -> Key -> Parser Float
parseFloat v field = asum
  [ v .: field
  , do
    s <- v .: field
    case readMaybe s :: Maybe Float of
      Nothing -> trace ("failed s=" ++ s) $ fail "not a number"
      Just x  -> return x
  ]

secondToUTC :: Real a => a -> UTCTime
secondToUTC = posixSecondsToUTCTime . realToFrac

parseUTCTime :: Object -> Key -> Parser UTCTime
parseUTCTime v field = asum
  [ v .: field
  , do
    s <- v .: field
    return $ secondToUTC (s :: Int)
  , do
    s <- v .: field
    case (readMaybe s :: Maybe Float) of
      Nothing -> trace ("failed s=" ++ s) $ fail "not a timestamp"
      Just x  -> return $ secondToUTC x
  , return $ secondToUTC 0
  ]

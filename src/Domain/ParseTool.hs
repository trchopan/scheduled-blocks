module Domain.ParseTool where

import           Data.Aeson                     ( (.:)
                                                , Key
                                                , Object
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Foldable                  ( asum )
import           Data.Int                       ( Int64 )
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

parseInt64 :: Object -> Key -> Parser Int64
parseInt64 v field = asum
  [ v .: field
  , do
    s <- v .: field
    case readMaybe s :: Maybe Int64 of
      Nothing -> trace ("failed s=" ++ s) $ fail "not a number"
      Just x  -> return x
  ]


parseInteger :: Object -> Key -> Parser Integer
parseInteger v field = asum
  [ v .: field
  , do
    s <- v .: field
    case readMaybe s :: Maybe Integer of
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

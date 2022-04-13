module Repository.Db where

import           Control.Exception              ( try )
import           Data.Aeson                     ( eitherDecode )
import qualified Data.ByteString.Lazy          as LBS
import           Domain.EpochSchedules          ( EpochSchedules )
import           System.IO.Error                ( isDoesNotExistError )
import           Text.Printf                    ( printf )


loadEpochSchedules :: FilePath -> IO (Either String EpochSchedules)
loadEpochSchedules path = do
  errOrByteString <- try $ LBS.readFile path
  case errOrByteString of
    Left err -> if isDoesNotExistError err
      then return $ Left $ printf "%s file not exist" path
      else error $ printf "something went wrong: %s" (show err)
    Right bs -> do
      case eitherDecode bs :: Either String EpochSchedules of
        Left  err -> return $ Left $ printf "%s cannot decode file: %s" path err
        Right dp  -> return $ Right dp

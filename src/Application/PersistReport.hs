module Application.PersistReport where

import           Application.CardanoHelpers     ( slotToTime )
import           Application.CommonHelpers      ( withStatusMessage )
import           Application.HistoryBlocks      ( HistoryBlocksArgs
                                                  ( HistoryBlocksArgs
                                                  )
                                                , checkLeaderSlots
                                                , getCheckLeaderArgs
                                                , historyBlocks
                                                , historyBlocksDummy
                                                )
import           Control.Monad                  ( forM_ )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import qualified Data.Text.Lazy.IO             as LIO
import           Domain.ArmadaNonce             ( ArmadaNonce
                                                  ( ArmadaNonce
                                                  , epochArmadaNonce
                                                  )
                                                )
import           Domain.EpochSchedules          ( EpochSchedules
                                                  ( EpochSchedules
                                                  , schedulesEpochSchedules
                                                  )
                                                , Schedule(Schedule)
                                                )
import           Repository.Api                 ( getCurrentNonce
                                                , requestAndDecode
                                                )
import           Repository.Db                  ( loadEpochSchedules )
import           System.TimeIt                  ( timeIt )
import           Text.Printf                    ( printf )


data PersistReportArgs = PersistReportArgs
  { blockFrostApi :: String
  , poolId        :: String
  , vrfSkey       :: String
  }


persistReport :: Int -> FilePath -> PersistReportArgs -> IO ()
persistReport fromEpoch filePath (PersistReportArgs blockFrostApi poolId vrfFilePath)
  = do
    armadaNonce <-
      withStatusMessage "Checking current network epoch..."
        $ requestAndDecode getCurrentNonce :: IO ArmadaNonce

    let currentEpoch = epochArmadaNonce armadaNonce

    forM_ [fromEpoch .. currentEpoch] $ \epoch -> do
      let epochFilePath = filePath ++ show epoch ++ ".json"
      existSchedule <- loadEpochSchedules epochFilePath
      case existSchedule of
        Right s -> do
          putStrLn $ printf
            "already check epoch %d\n%s\n"
            epoch
            (decodeUtf8 $ encodePretty $ schedulesEpochSchedules s)
        Left err -> do
          putStrLn err
          lArgs <- getCheckLeaderArgs
            (HistoryBlocksArgs blockFrostApi epoch poolId vrfFilePath)
          leaderSlots <- timeIt $ checkLeaderSlots lArgs
          let schedules =
                map (\slot -> Schedule slot (slotToTime slot)) leaderSlots
              newDbReport = EpochSchedules epoch (length leaderSlots) schedules
          LIO.writeFile (filePath ++ show epoch ++ ".json")
            $ decodeUtf8
            $ encodePretty newDbReport

    putStrLn "Finished"

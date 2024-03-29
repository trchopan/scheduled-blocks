module Application.HistoryBlocks
  ( historyBlocks
  , HistoryBlocksArgs(HistoryBlocksArgs)
  , checkLeaderSlots
  , getCheckLeaderArgs
  ) where


import           Application.CardanoHelpers     ( isSlotLeader
                                                , mkSigmaOfF
                                                , slotToTime
                                                )
import           Application.CommonHelpers      ( PrintInfomation
                                                  ( PrintInfomation
                                                  )
                                                , decodeB16OrError
                                                , epochInfoRequest
                                                , epochParamRequest
                                                , firstShellyBlockRequest
                                                , genesisRequest
                                                , latestEpochRequest
                                                , percentageProcessBar
                                                , poolHistoryRequest
                                                , poolInfoRequest
                                                , printInformation
                                                , textToBS
                                                , timeToString
                                                )
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad                  ( when )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.UTF8           ( fromString )
import           Data.Int                       ( Int64 )
import           Data.List                      ( find )
import           Data.Time                      ( getCurrentTimeZone )
import           Domain.BlockInfo               ( BlockInfo(slotBlockInfo) )
import           Domain.BlockchainGenesis       ( BlockchainGenesis
                                                  ( activeSlotsCoefficientBlockchainGenesis
                                                  , epochLengthBlockchainGenesis
                                                  , slotLengthBlockchainGenesis
                                                  )
                                                )
import           Domain.EpochInfo               ( EpochInfo
                                                  ( activeStakeEpochInfo
                                                  )
                                                )
import           Domain.EpochParameter          ( EpochParameter
                                                  ( epochEpochParameter
                                                  , nonceEpochParameter
                                                  )
                                                )
import           Domain.PoolHistory             ( PoolHistory
                                                  ( activeSizePoolHistory
                                                  , activeStakePoolHistory
                                                  , epochPoolHistory
                                                  )
                                                )
import           Domain.PoolInfo                ( PoolInfo
                                                  ( activeSizePoolInfo
                                                  , activeStakePoolInfo
                                                  )
                                                )
import           Repository.KeyFile             ( loadVrfSkey
                                                , poolVrfSkey
                                                )
import           System.ProgressBar             ( Progress(Progress)
                                                , updateProgress
                                                )
import           System.TimeIt                  ( timeIt )
import           Text.Printf                    ( printf )



data HistoryBlocksArgs = HistoryBlocksArgs
  { blockFrostApi :: String
  , epoch         :: Int
  , poolId        :: String
  , vrfFilePath   :: String
  }


data CheckLeaderArgs = CheckLeaderArgs
  { vrfSkeyBytes     :: ByteString
  , nonceBytes       :: ByteString
  , sigmaOfF         :: Float
  , firstSlotOfEpoch :: Int64
  , epochLength      :: Int64
  }


getCheckLeaderArgs :: HistoryBlocksArgs -> IO CheckLeaderArgs
getCheckLeaderArgs (HistoryBlocksArgs blockFrostApi epoch poolId vrfFilePath) =
  do
    putStrLn $ concat $ replicate 80 "="
    latestEpoch <- latestEpochRequest blockFrostApi
    let currentEpoch = epochEpochParameter latestEpoch

    when
      (epoch > currentEpoch)
      (error $ printf
        "epoch %d is in the future. Current epoch is %d. Please use `next` command."
        epoch
        currentEpoch
      )

    (poolSigma, poolActiveStake) <- if currentEpoch == epoch
      then do
        poolInfo <- poolInfoRequest blockFrostApi poolId
        return (activeSizePoolInfo poolInfo, activeStakePoolInfo poolInfo)
      else do
        poolHistories <- poolHistoryRequest blockFrostApi poolId
        case find (\ph -> epochPoolHistory ph == epoch) poolHistories of
          Nothing ->
            error ("Cannot find pool history for epoch " ++ show epoch)
          Just ph ->
            return (activeSizePoolHistory ph, activeStakePoolHistory ph)

    epochParams      <- epochParamRequest blockFrostApi epoch
    epochInfo        <- epochInfoRequest blockFrostApi epoch
    genesis          <- genesisRequest blockFrostApi
    firstShellyBlock <- firstShellyBlockRequest blockFrostApi

    let
      nonce           = nonceEpochParameter epochParams
      activeStake     = activeStakeEpochInfo epochInfo
      epochLength     = epochLengthBlockchainGenesis genesis
      activeSlotCoeff = activeSlotsCoefficientBlockchainGenesis genesis
      slotLength      = slotLengthBlockchainGenesis genesis
      firstShellySlot = slotBlockInfo firstShellyBlock
      firstSlotOfEpoch =
        firstShellySlot + (fromIntegral epoch - 211) * epochLength

    printInformation
      (PrintInfomation nonce
                       activeSlotCoeff
                       epochLength
                       slotLength
                       firstSlotOfEpoch
                       activeStake
                       poolActiveStake
                       poolSigma
      )

    vrfSignKey <- loadVrfSkey vrfFilePath
    let vrfSkeyBytes = (decodeB16OrError . fromString) (poolVrfSkey vrfSignKey)
        nonceBytes   = decodeB16OrError $ textToBS nonce
        sigmaOfF     = mkSigmaOfF activeSlotCoeff poolSigma

    return $ CheckLeaderArgs vrfSkeyBytes
                             nonceBytes
                             sigmaOfF
                             firstSlotOfEpoch
                             epochLength


-- ([<LeaderSlot>], <Numer of finished slots>)
type Result = TVar ([Int64], Int64)

addToResult :: CheckLeaderArgs -> Result -> Int64 -> STM ()
addToResult args result slot = do
  (leaderSlots, endCtr) <- readTVar result

  let
    (CheckLeaderArgs vrfSkeyBytes nonceBytes sigmaOfF firstSlotOfEpoch epochLength)
      = args
    newLeaderSlots = if isSlotLeader sigmaOfF nonceBytes vrfSkeyBytes slot
      then leaderSlots ++ [slot]
      else leaderSlots

  writeTVar result (newLeaderSlots, endCtr + 1)


waitForResult :: Result -> Int64 -> STM [Int64]
waitForResult result limit = do
  (leaderSlots, endCtr) <- readTVar result
  if endCtr < limit then retry else return leaderSlots


checkLeaderSlotsConcurrent :: CheckLeaderArgs -> IO ()
checkLeaderSlotsConcurrent lArgs = do
  let
    (CheckLeaderArgs vrfSkeyBytes nonceBytes sigmaOfF firstSlotOfEpoch epochLength)
      = lArgs
    slotsOfEpoch = [firstSlotOfEpoch .. firstSlotOfEpoch + epochLength]

  result <- newTVarIO ([], 0)
  mapM_ (forkIO . atomically . addToResult lArgs result) slotsOfEpoch

  leaderSlots <- atomically
    $ waitForResult result (fromIntegral $ length slotsOfEpoch)

  putStrLn $ printf "Slots: %s" (show leaderSlots)


checkLeaderSlots :: CheckLeaderArgs -> IO [Int64]
checkLeaderSlots lArgs = do
  let
    (CheckLeaderArgs vrfSkeyBytes nonceBytes sigmaOfF firstSlotOfEpoch epochLength)
      = lArgs
    slotsOfEpoch = [firstSlotOfEpoch .. firstSlotOfEpoch + epochLength]
  tz     <- getCurrentTimeZone
  pb     <- percentageProcessBar

  result <- mapM
    (\slot -> do
      let isLeader = isSlotLeader sigmaOfF nonceBytes vrfSkeyBytes slot
      when isLeader $ putStrLn $ printf "Slot %d block assigned. Time %s\n"
                                        slot
                                        (timeToString (slotToTime slot) tz)

      let percentDone =
            round
              $ 100
              * toRational (slot - firstSlotOfEpoch)
              / toRational epochLength
      updateProgress pb (\_ -> Progress percentDone 100 ())

      return (slot, isLeader)
    )
    slotsOfEpoch

  return $ map fst $ filter snd result

historyBlocks :: HistoryBlocksArgs -> IO ()
historyBlocks hArgs = do
  lArgs <- getCheckLeaderArgs hArgs
  timeIt $ checkLeaderSlotsConcurrent lArgs
  -- timeIt $ checkLeaderSlots lArgs
  return ()

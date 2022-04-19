{-# LANGUAGE LambdaCase #-}

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
import           Application.CommonHelpers      ( decodeB16OrError
                                                , percentageProcessBar
                                                , prettyInt
                                                , textToBS
                                                , timeToString
                                                , withStatusMessage
                                                )
import           Control.Concurrent             ( Chan
                                                , ThreadId
                                                , forkIO
                                                , newChan
                                                , readChan
                                                , writeChan
                                                )
import           Control.Monad                  ( replicateM_
                                                , when
                                                )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.UTF8           ( fromString )
import           Data.IORef                     ( IORef
                                                , atomicModifyIORef
                                                , newIORef
                                                )
import           Data.Int                       ( Int64 )
import           Data.List                      ( find )
import           Data.Time                      ( getCurrentTimeZone )
import           Domain.ArmadaNonce             ( ArmadaNonce(epochArmadaNonce)
                                                )
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
                                                  ( nonceEpochParameter
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
import           Repository.Api                 ( getBlockchainGenesis
                                                , getCurrentNonce
                                                , getEpochInfo
                                                , getEpochParam
                                                , getFirstShellyBlock
                                                , getPoolHistory
                                                , getPoolInfo
                                                , requestAndDecode
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
    armadaNonce <-
      withStatusMessage "Checking current network epoch..."
        $ requestAndDecode getCurrentNonce :: IO ArmadaNonce

    let currentEpoch = epochArmadaNonce armadaNonce

    when
      (epoch > currentEpoch)
      (error $ printf
        "epoch %d is in the future. Current epoch is %d. Please use `next` command."
        epoch
        currentEpoch
      )

    (poolSigma, poolActiveStake) <- if currentEpoch == epoch
      then do
        poolInfo <-
          withStatusMessage "Checking Pool Sigma of Current Epoch..."
          $ requestAndDecode
          $ getPoolInfo blockFrostApi poolId :: IO PoolInfo
        return (activeSizePoolInfo poolInfo, activeStakePoolInfo poolInfo)
      else do
        poolHistories <-
          withStatusMessage "Checking Pool Sigma from Pool History..."
          $ requestAndDecode
          $ getPoolHistory blockFrostApi poolId :: IO [PoolHistory]
        case find (\ph -> epochPoolHistory ph == epoch) poolHistories of
          Nothing ->
            error ("Cannot find pool history for epoch " ++ show epoch)
          Just ph ->
            return (activeSizePoolHistory ph, activeStakePoolHistory ph)

    epochParams <-
      withStatusMessage "Checking epoch parameters..."
      $ requestAndDecode
      $ getEpochParam blockFrostApi epoch :: IO EpochParameter

    epochInfo <-
      withStatusMessage "Checking epoch info for active stake..."
      $ requestAndDecode
      $ getEpochInfo blockFrostApi epoch :: IO EpochInfo

    genesis <-
      withStatusMessage "Checking network genenis..."
      $ requestAndDecode
      $ getBlockchainGenesis blockFrostApi :: IO BlockchainGenesis

    firstShellyBlock <-
      withStatusMessage "Checking first Shelly block..."
      $ requestAndDecode
      $ getFirstShellyBlock blockFrostApi :: IO BlockInfo

    let
      nonce           = nonceEpochParameter epochParams
      activeStake     = activeStakeEpochInfo epochInfo
      epochLength     = epochLengthBlockchainGenesis genesis
      activeSlotCoeff = activeSlotsCoefficientBlockchainGenesis genesis
      slotLength      = slotLengthBlockchainGenesis genesis
      firstShellySlot = slotBlockInfo firstShellyBlock
      firstSlotOfEpoch =
        firstShellySlot + (fromIntegral epoch - 211) * epochLength

    printf "Epoch: %d\n"                     epoch
    printf "Nonce: %s\n"                     nonce
    printf "Active Slot Coefficient: %.3f\n" activeSlotCoeff
    printf "Epoch Length: %d\n"              epochLength
    printf "Slot Length: %d\n"               slotLength
    printf "First Slot of Epoch: %d\n"       firstSlotOfEpoch
    printf "Last Slot of Epoch: %d\n"        (firstSlotOfEpoch + epochLength)
    printf "Active Stake (epoch %s): %s\n" (show epoch) (prettyInt activeStake)
    printf "Pool Active Stake: %s\n" (prettyInt poolActiveStake)
    printf "Pool Sigma: %.9f\n"      poolSigma

    vrfSignKey <- loadVrfSkey vrfFilePath
    let vrfSkeyBytes = (decodeB16OrError . fromString) (poolVrfSkey vrfSignKey)
        nonceBytes   = decodeB16OrError $ textToBS nonce
        sigmaOfF     = mkSigmaOfF activeSlotCoeff poolSigma

    return $ CheckLeaderArgs vrfSkeyBytes
                             nonceBytes
                             sigmaOfF
                             firstSlotOfEpoch
                             epochLength


worker
  :: CheckLeaderArgs
  -> IORef [Int64]
  -> Chan (Int, Int64, Bool)
  -> Int
  -> IO ThreadId
worker (CheckLeaderArgs vrfSkeyBytes nonceBytes sigmaOfF firstSlotOfEpoch epochLength) requestsRef responseChan workerId
  = forkIO $ do
    let loop = do
          mint <- atomicModifyIORef requestsRef $ \case
            []          -> ([], Nothing)
            slot : rest -> (rest, Just slot)

          case mint of
            Nothing   -> return ()
            Just slot -> do
              writeChan
                responseChan
                ( workerId
                , slot
                , isSlotLeader sigmaOfF nonceBytes vrfSkeyBytes slot
                )
              loop

    -- Kick off the loop
    loop


checkLeaderSlotsConcurrent :: CheckLeaderArgs -> IO ()
checkLeaderSlotsConcurrent lArgs = do
  let
    (CheckLeaderArgs vrfSkeyBytes nonceBytes sigmaOfF firstSlotOfEpoch epochLength)
      = lArgs
    slotsOfEpoch = [firstSlotOfEpoch .. firstSlotOfEpoch + epochLength]
    workerCount  = 1000
  requestsRef  <- newIORef slotsOfEpoch
  responseChan <- newChan
  tz           <- getCurrentTimeZone
  pb           <- percentageProcessBar

  mapM_ (worker lArgs requestsRef responseChan) [1 .. workerCount]

  replicateM_ (length slotsOfEpoch) $ do
    (workerId, slot, isLeader) <- readChan responseChan
    when isLeader $ putStrLn $ printf "Slot %d block assigned. Time %s\n"
                                      slot
                                      (timeToString (slotToTime slot) tz)

    let percentDone =
          round
            $ 100
            * toRational (slot - firstSlotOfEpoch)
            / toRational epochLength
    updateProgress pb (\_ -> Progress percentDone 100 ())


checkLeaderSlots :: CheckLeaderArgs -> IO [Int64]
checkLeaderSlots (CheckLeaderArgs vrfSkeyBytes nonceBytes sigmaOfF firstSlotOfEpoch epochLength)
  = do
    let slotsOfEpoch = [firstSlotOfEpoch .. firstSlotOfEpoch + epochLength]
    tz     <- getCurrentTimeZone
    pb     <- percentageProcessBar

    result <- mapM
      (\slot -> do
        let isLeader = isSlotLeader sigmaOfF nonceBytes vrfSkeyBytes slot
        when (isSlotLeader sigmaOfF nonceBytes vrfSkeyBytes slot)
          $ putStrLn
          $ printf "Slot %d block assigned. Time %s\n"
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

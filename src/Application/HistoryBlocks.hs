module Application.HistoryBlocks
  ( historyBlocks
  , HistoryBlocksArgs(HistoryBlocksArgs)
  ) where

import           Application.CardanoHelpers     ( isSlotLeader
                                                , mkSigmaOfF
                                                , slotToTime
                                                )
import           Application.CommonHelpers      ( decodeBS
                                                , percentageProcessBar
                                                , prettyInt
                                                , textToBS
                                                , timeToString
                                                , withStatusMessage
                                                )
import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Data.ByteString.UTF8           ( fromString )
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
import           Text.Printf                    ( printf )



data HistoryBlocksArgs = HistoryBlocksArgs
  { blockFrostApi :: String
  , epoch         :: Int
  , poolId        :: String
  , vrfSkey       :: String
  }



historyBlocks :: HistoryBlocksArgs -> IO ()
historyBlocks (HistoryBlocksArgs blockFrostApi epoch poolId vrfFilePath) = do
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
      let poolHistory =
            find (\ph -> epochPoolHistory ph == epoch) poolHistories
      case poolHistory of
        Nothing -> error ("Cannot find pool history for epoch " ++ show epoch)
        Just ph -> return (activeSizePoolHistory ph, activeStakePoolHistory ph)

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
    requestAndDecode $ getFirstShellyBlock blockFrostApi :: IO BlockInfo

  let
    nonce           = nonceEpochParameter epochParams
    activeStake     = activeStakeEpochInfo epochInfo
    epochLength     = epochLengthBlockchainGenesis genesis
    activeSlotCoeff = activeSlotsCoefficientBlockchainGenesis genesis
    slotLength      = slotLengthBlockchainGenesis genesis
    firstShellySlot = slotBlockInfo firstShellyBlock
    firstSlotOfEpoch =
      firstShellySlot + (fromIntegral epoch - 211) * epochLength

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
  tz         <- getCurrentTimeZone
  let vrfSkeyBytes = (decodeBS . fromString) (poolVrfSkey vrfSignKey)
      decodedNonce = decodeBS $ textToBS nonce
      slotsOfEpoch = [firstSlotOfEpoch .. firstSlotOfEpoch + epochLength]
      sigmaOfF     = mkSigmaOfF activeSlotCoeff poolSigma

  pb <- percentageProcessBar

  forM_ slotsOfEpoch $ \slot -> do
    when (isSlotLeader sigmaOfF decodedNonce vrfSkeyBytes slot)
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

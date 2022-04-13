module Application.NextBlocks
  ( nextBlocks
  , NextBlocksArgs(NextBlocksArgs)
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
import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Data.ByteString.UTF8           ( fromString )
import           Data.Time                      ( getCurrentTimeZone )
import           Domain.ArmadaNonce             ( ArmadaNonce
                                                  ( epochArmadaNonce
                                                  , nonceArmadaNonce
                                                  )
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
import           Domain.PoolInfo                ( PoolInfo
                                                  ( activeSizePoolInfo
                                                  , activeStakePoolInfo
                                                  )
                                                )
import           Repository.Api                 ( getBlockchainGenesis
                                                , getEpochInfo
                                                , getFirstShellyBlock
                                                , getNextNonce
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



data NextBlocksArgs = NextBlocksArgs
  { blockFrostApi :: String
  , poolId        :: String
  , vrfSkey       :: String
  }

nextBlocks :: NextBlocksArgs -> IO ()
nextBlocks (NextBlocksArgs blockFrostApi poolId vrfFilePath) = do
  armadaNonce <-
    withStatusMessage "Checking next network epoch..."
      $ requestAndDecode getNextNonce :: IO ArmadaNonce
  let nonce     = nonceArmadaNonce armadaNonce
      nextEpoch = epochArmadaNonce armadaNonce

  putStrLn $ printf "Next epoch is %d" nextEpoch

  poolInfo <-
    withStatusMessage "Checking Current Pool Sigma..."
    $ requestAndDecode
    $ getPoolInfo blockFrostApi poolId :: IO PoolInfo
  let (poolSigma, poolActiveStake) =
        (activeSizePoolInfo poolInfo, activeStakePoolInfo poolInfo)

  epochInfo <-
    withStatusMessage "Checking epoch info for active stake..."
    $ requestAndDecode
    $ getEpochInfo blockFrostApi (nextEpoch - 1) :: IO EpochInfo

  genesis <-
    withStatusMessage "Checking network genenis..."
    $ requestAndDecode
    $ getBlockchainGenesis blockFrostApi :: IO BlockchainGenesis

  firstShellyBlock <-
    requestAndDecode $ getFirstShellyBlock blockFrostApi :: IO BlockInfo

  let activeStake     = activeStakeEpochInfo epochInfo
      epochLength     = epochLengthBlockchainGenesis genesis
      activeSlotCoeff = activeSlotsCoefficientBlockchainGenesis genesis
      slotLength      = slotLengthBlockchainGenesis genesis
      firstShellySlot = slotBlockInfo firstShellyBlock
      firstSlotOfEpoch =
        firstShellySlot + (fromIntegral nextEpoch - 211) * epochLength

  printf "Nonce: %s\n"                     nonce
  printf "Active Slot Coefficient: %.3f\n" activeSlotCoeff
  printf "Epoch Length: %d\n"              epochLength
  printf "Slot Length: %d\n"               slotLength
  printf "First Slot of Epoch: %d\n"       firstSlotOfEpoch
  printf "Last Slot of Epoch: %d\n"        (firstSlotOfEpoch + epochLength)
  printf "Active Stake (epoch %s): %s\n"
         (show nextEpoch)
         (prettyInt activeStake)
  printf "Pool Active Stake: %s\n" (prettyInt poolActiveStake)
  printf "Pool Sigma: %.9f\n"      poolSigma

  vrfSignKey <- loadVrfSkey vrfFilePath
  tz         <- getCurrentTimeZone

  let vrfSkeyBytes = (decodeB16OrError . fromString) (poolVrfSkey vrfSignKey)
      decodedNonce = decodeB16OrError $ textToBS nonce
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

module Application.NextBlocks
  ( nextBlocks
  , NextBlocksArgs(NextBlocksArgs)
  ) where

import           Application.CardanoHelpers     ( isSlotLeader
                                                , mkSigmaOfF
                                                , slotToTime
                                                )
import           Application.CommonHelpers      ( PrintInfomation
                                                  ( PrintInfomation
                                                  )
                                                , armadaNonceRequest
                                                , decodeB16OrError
                                                , epochInfoRequest
                                                , firstShellyBlockRequest
                                                , genesisRequest
                                                , percentageProcessBar
                                                , poolInfoRequest
                                                , printInformation
                                                , textToBS
                                                , timeToString
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
  armadaNonce <- armadaNonceRequest
  let nonce     = nonceArmadaNonce armadaNonce
      nextEpoch = epochArmadaNonce armadaNonce

  putStrLn $ printf "Next epoch is %d" nextEpoch

  poolInfo <- poolInfoRequest blockFrostApi poolId
  let poolSigma       = activeSizePoolInfo poolInfo
      poolActiveStake = activeStakePoolInfo poolInfo

  epochInfo        <- epochInfoRequest blockFrostApi (nextEpoch - 1)
  genesis          <- genesisRequest blockFrostApi
  firstShellyBlock <- firstShellyBlockRequest blockFrostApi

  let activeStake     = activeStakeEpochInfo epochInfo
      epochLength     = epochLengthBlockchainGenesis genesis
      activeSlotCoeff = activeSlotsCoefficientBlockchainGenesis genesis
      slotLength      = slotLengthBlockchainGenesis genesis
      firstShellySlot = slotBlockInfo firstShellyBlock
      firstSlotOfEpoch =
        firstShellySlot + (fromIntegral nextEpoch - 211) * epochLength

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

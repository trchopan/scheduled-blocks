module Application.MyApp
  ( epochSchedule
  , epochBlockSchedule
  ) where

import           Data.Aeson                     ( eitherDecode )
import qualified Data.Text                     as T
import           Data.Text.Format.Numbers       ( PrettyCfg(PrettyCfg)
                                                , prettyF
                                                )
import           Domain.BlockInfo               ( BlockInfo(slotBlockInfo) )
import           Domain.BlockchainGenesis       ( BlockchainGenesis
                                                  ( activeSlotsCoefficientBlockchainGenesis
                                                  , epochLengthBlockchainGenesis
                                                  , slotLengthBlockchainGenesis
                                                  )
                                                )
import           Domain.CardanoGlobal           ( Cardano
                                                , nextEpochs
                                                )
import           Domain.EpochInfo               ( EpochInfo
                                                  ( activeStakeEpochInfo
                                                  )
                                                )
import           Domain.EpochParameter          ( EpochParameter
                                                  ( nonceEpochParameter
                                                  )
                                                )
import           Domain.PoolInfo                ( PoolInfo
                                                  ( activeSizePoolInfo
                                                  , activeStakePoolInfo
                                                  )
                                                )
import           Network.HTTP.Simple            ( getResponseBody
                                                , httpLBS
                                                )
import           Repository.Api                 ( getBlockchainGenesis
                                                , getEpochInfo
                                                , getEpochParam
                                                , getFirstShellyBlock
                                                , getGlobal
                                                , getPoolInfo
                                                , requestAndDecode
                                                )
import           System.Exit                    ( ExitCode(ExitFailure)
                                                , exitWith
                                                )
import           Text.Printf                    ( printf )


epochSchedule :: IO ()
epochSchedule = do
  globalResBs <- httpLBS getGlobal
  let eitherGlobal =
        eitherDecode (getResponseBody globalResBs) :: Either String Cardano
  case eitherGlobal of
    Left  err     -> print ("Cannot get global. Err: " ++ err)
    Right cardano -> putStr $ unlines $ nextEpochs cardano 3

prettyInt :: Integer -> T.Text
prettyInt = prettyF (PrettyCfg 0 (Just ',') '.') . toRational . toInteger

epochBlockSchedule :: String -> String -> Int -> IO ()
epochBlockSchedule blockFrostApi poolId epochNo = do
  epochParams <-
    requestAndDecode $ getEpochParam blockFrostApi epochNo :: IO EpochParameter

  epochInfo <-
    requestAndDecode $ getEpochInfo blockFrostApi epochNo :: IO EpochInfo

  genesis <-
    requestAndDecode $ getBlockchainGenesis blockFrostApi :: IO
      BlockchainGenesis

  poolInfo <- requestAndDecode $ getPoolInfo blockFrostApi poolId :: IO PoolInfo

  firstShellyBlock <-
    requestAndDecode $ getFirstShellyBlock blockFrostApi :: IO BlockInfo

  let nonce                 = nonceEpochParameter epochParams
      activeStake           = activeStakeEpochInfo epochInfo
      epochLength           = epochLengthBlockchainGenesis genesis
      activeSlotCoefficient = activeSlotsCoefficientBlockchainGenesis genesis
      slotLength            = slotLengthBlockchainGenesis genesis
      poolSigma             = activeSizePoolInfo poolInfo
      poolActiveStake       = activeStakePoolInfo poolInfo
      firstShellySlot       = slotBlockInfo firstShellyBlock
      firstSlotOfEpoch      = firstShellySlot + (epochNo - 221) * epochLength

  printf "Nonce: %s\n"                     nonce
  printf "Active Slot Coefficient: %.3f\n" activeSlotCoefficient
  printf "Epoch Length: %d\n"              epochLength
  printf "Slot Length: %d\n"               slotLength
  printf "First Slot of Epoch: %d\n"       firstSlotOfEpoch
  printf "Active Stake (epoch %s): %s\n" (show epochNo) (prettyInt activeStake)
  printf "Pool Active Stake: %s\n" (prettyInt poolActiveStake)
  printf "Pool Sigma: %.9f\n"      poolSigma

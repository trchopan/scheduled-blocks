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


handleLeftResponse :: String -> Either String b -> IO ()
handleLeftResponse prefix e = case e of
  Left err -> do
    print ("Error handle " ++ prefix ++ ": " ++ err)
    exitWith (ExitFailure 1)
  Right _ -> return ()


prettyInt :: Integer -> T.Text
prettyInt = prettyF (PrettyCfg 0 (Just ',') '.') . toRational . toInteger

epochBlockSchedule :: String -> String -> Int -> IO ()
epochBlockSchedule blockFrostApi poolId epochNo = do
  let _blockFrostApi = blockFrostApi

  epochParamsResBs <- httpLBS $ getEpochParam _blockFrostApi epochNo
  let eitherEpochParams =
        eitherDecode (getResponseBody epochParamsResBs) :: Either
            String
            EpochParameter

  handleLeftResponse "Getting Epoch Params" eitherEpochParams

  let nonce = case eitherEpochParams of
        Left  _ -> ""
        Right e -> nonceEpochParameter e

  epochInfoResBs <- httpLBS $ getEpochInfo _blockFrostApi epochNo
  let eitherEpochInfo =
        eitherDecode (getResponseBody epochInfoResBs) :: Either String EpochInfo

  handleLeftResponse "Getting EpochInfo" eitherEpochInfo

  let activeStake = case eitherEpochInfo of
        Left  _ -> 0
        Right e -> activeStakeEpochInfo e


  genesisResBs <- httpLBS $ getBlockchainGenesis _blockFrostApi
  let eitherBlockGenesis =
        eitherDecode (getResponseBody genesisResBs) :: Either
            String
            BlockchainGenesis

  handleLeftResponse "Getting Genesis" eitherBlockGenesis

  let (epochLength, activeSlotCoefficient, slotLength) =
        case eitherBlockGenesis of
          Left _ -> (0, 0, 0)
          Right e ->
            ( epochLengthBlockchainGenesis e
            , activeSlotsCoefficientBlockchainGenesis e
            , slotLengthBlockchainGenesis e
            )

  poolInfoBs <- httpLBS $ getPoolInfo _blockFrostApi poolId
  let eitherPoolInfo =
        eitherDecode (getResponseBody poolInfoBs) :: Either String PoolInfo

  handleLeftResponse "Getting PoolInfo" eitherPoolInfo

  let (poolSigma, poolActiveStake) = case eitherPoolInfo of
        Left  _ -> (0, 0)
        Right e -> (activeSizePoolInfo e, activeStakePoolInfo e)

  firstShellyBlockBs <- httpLBS $ getFirstShellyBlock _blockFrostApi
  let eitherFirstShellyBlock =
        eitherDecode (getResponseBody firstShellyBlockBs) :: Either
            String
            BlockInfo

  handleLeftResponse "Getting First Shelly Block" eitherFirstShellyBlock

  let firstShellyBlock = case eitherFirstShellyBlock of
        Left  _ -> 0
        Right e -> slotBlockInfo e
      firstSlotOfEpoch = firstShellyBlock + (epochNo - 221) * epochLength

  printf "Nonce: %s\n"                     nonce
  printf "Active Slot Coefficient: %.3f\n" activeSlotCoefficient
  printf "Epoch Length: %d\n"              epochLength
  printf "Slot Length: %d\n"               slotLength
  printf "First Slot of Epoch: %d\n"       firstSlotOfEpoch
  printf "Active Stake (epoch %s): %s\n" (show epochNo) (prettyInt activeStake)
  printf "Pool Active Stake: %s\n" (prettyInt poolActiveStake)
  printf "Pool Sigma: %.9f\n"      poolSigma

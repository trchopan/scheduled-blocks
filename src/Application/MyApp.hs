module Application.MyApp
  ( epochSchedule
  , epochBlockSchedule
  ) where

import           Application.Cardano            ( bytestringToNatural
                                                , certNatMax
                                                , decodeBS
                                                , formatLocalTime
                                                , hashBlake2b
                                                , mkSeed
                                                , mkSigmaOfF
                                                , nextEpochs
                                                , prettyInt
                                                , seedLBytes
                                                , slotToSeedBytes
                                                , slotToTime
                                                , textToBS
                                                )
import           Control.Monad                  ( when )
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.UTF8           ( ByteString
                                                , fromString
                                                )
import           Data.Int                       ( Int64 )
import           Domain.ArmadaNonce             ( ArmadaNonce(epochArmadaNonce)
                                                )
import           Domain.BlockInfo               ( BlockInfo(slotBlockInfo) )
import           Domain.BlockchainGenesis       ( BlockchainGenesis
                                                  ( activeSlotsCoefficientBlockchainGenesis
                                                  , epochLengthBlockchainGenesis
                                                  , slotLengthBlockchainGenesis
                                                  )
                                                )
import           Domain.CardanoGlobal           ( Cardano )
import           Domain.EpochInfo               ( EpochInfo
                                                  ( activeStakeEpochInfo
                                                  )
                                                )
import           Domain.EpochParameter          ( EpochParameter
                                                  ( nonceEpochParameter
                                                  )
                                                )
import           Domain.PoolHistory             ( PoolHistory (activeSizePoolHistory, activeStakePoolHistory, epochPoolHistory) )
import           Domain.PoolInfo                ( PoolInfo
                                                  ( activeSizePoolInfo
                                                  , activeStakePoolInfo
                                                  )
                                                )
import           Network.HTTP.Simple            ( getResponseBody
                                                , httpLBS
                                                )
import           Repository.Api                 ( getBlockchainGenesis
                                                , getCurrentNonce
                                                , getEpochInfo
                                                , getEpochParam
                                                , getFirstShellyBlock
                                                , getGlobal
                                                , getPoolHistory
                                                , getPoolInfo
                                                , requestAndDecode
                                                )
import           Repository.Cardano.Crypto.VRF.Praos
                                                ( SignKey
                                                , outputBytes
                                                , outputFromProof
                                                , prove
                                                , skFromBytes
                                                )
import           Repository.KeyFile             ( loadVrfSkey
                                                , poolVrfSkey
                                                )
import           Text.Printf                    ( printf )
import           Data.List ( find )



epochSchedule :: IO ()
epochSchedule = do
  globalResBs <- httpLBS getGlobal
  let eitherGlobal =
        eitherDecode (getResponseBody globalResBs) :: Either String Cardano
  case eitherGlobal of
    Left  err     -> print ("Cannot get global. Err: " ++ err)
    Right cardano -> putStr $ unlines $ nextEpochs cardano 3

isSlotLeader :: Float -> ByteString -> SignKey -> Int64 -> IO Bool
isSlotLeader sigmaOfF nonce vrfKey slotNumber = do
  let seedBytes       = slotToSeedBytes slotNumber nonce
      hashedSeedBytes = hashBlake2b seedBytes
      seed            = mkSeed seedLBytes hashedSeedBytes
      maybeProof      = prove vrfKey seed
      proofHash = maybe "" (maybe "" outputBytes . outputFromProof) maybeProof
      certNat         = bytestringToNatural proofHash
      denominator     = certNatMax - certNat
      q = realToFrac (toRational certNatMax / toRational denominator)

  when (q <= sigmaOfF) $ printf "Slot %d block assigned. Time %s\n"
                                slotNumber
                                (formatLocalTime $ slotToTime slotNumber)

  return $ q <= sigmaOfF


epochBlockSchedule :: String -> String -> Int -> String -> IO ()
epochBlockSchedule blockFrostApi poolId epochNo vrfFilePath = do
  putStrLn $ printf "Checking current epoch"
  armadaNonce <- requestAndDecode getCurrentNonce :: IO ArmadaNonce

  let currentEpoch = epochArmadaNonce armadaNonce

  putStrLn $ printf "Checking epoch parameters"
  epochParams <-
    requestAndDecode $ getEpochParam blockFrostApi epochNo :: IO EpochParameter

  putStrLn $ printf "Checking epoch info"
  epochInfo <-
    requestAndDecode $ getEpochInfo blockFrostApi epochNo :: IO EpochInfo

  putStrLn $ printf "Checking network genenis"
  genesis <-
    requestAndDecode $ getBlockchainGenesis blockFrostApi :: IO
      BlockchainGenesis

  putStrLn $ printf "Checking pool Sigma and Active Stake"
  (poolSigma, poolActiveStake) <- if currentEpoch == epochNo
    then do
      poolInfo <-
        requestAndDecode $ getPoolInfo blockFrostApi poolId :: IO PoolInfo
      return (activeSizePoolInfo poolInfo, activeStakePoolInfo poolInfo)
    else do
      poolHistories <-
        requestAndDecode $ getPoolHistory blockFrostApi poolId :: IO
          [PoolHistory]
      let poolHistory = find (\ph -> epochPoolHistory ph == epochNo) poolHistories
      case poolHistory of
        Nothing -> error ("Cannot find pool history for epoch " ++ show epochNo)
        Just ph -> return ( activeSizePoolHistory ph , activeStakePoolHistory ph)


  firstShellyBlock <-
    requestAndDecode $ getFirstShellyBlock blockFrostApi :: IO BlockInfo

  let
    currentEpoch    = epochArmadaNonce armadaNonce
    nonce           = nonceEpochParameter epochParams
    activeStake     = activeStakeEpochInfo epochInfo
    epochLength     = epochLengthBlockchainGenesis genesis
    activeSlotCoeff = activeSlotsCoefficientBlockchainGenesis genesis
    slotLength      = slotLengthBlockchainGenesis genesis
    firstShellySlot = slotBlockInfo firstShellyBlock
    firstSlotOfEpoch =
      firstShellySlot + (fromIntegral epochNo - 211) * epochLength

  -- let
  --   currentEpoch    = epochArmadaNonce armadaNonce
  --   nonce = "97be25ab0a46a6537faaf32f882de17611c897f32c7eeef12f53a176b225e461"
  --   activeStake     = 23537719498083358
  --   epochLength     = 432000
  --   activeSlotCoeff = 0.050
  --   slotLength      = 1 :: Int
  --   poolSigma       = 0.00003236318494333271
  --   poolActiveStake = 916392141238
  --   firstShellySlot = 5788800 :: Int64
  --   firstSlotOfEpoch =
  --     firstShellySlot + (fromIntegral epochNo - 211) * epochLength



  printf "Nonce: %s\n"                     nonce
  printf "Active Slot Coefficient: %.3f\n" activeSlotCoeff
  printf "Epoch Length: %d\n"              epochLength
  printf "Slot Length: %d\n"               slotLength
  printf "First Slot of Epoch: %d\n"       firstSlotOfEpoch
  printf "Last Slot of Epoch: %d\n"        (firstSlotOfEpoch + epochLength)
  printf "Active Stake (epoch %s): %s\n" (show epochNo) (prettyInt activeStake)
  printf "Pool Active Stake: %s\n" (prettyInt poolActiveStake)
  printf "Pool Sigma: %.9f\n"      poolSigma

  vrfSignKey <- loadVrfSkey vrfFilePath
  let vrfKeyBytes  = (decodeBS . fromString) (poolVrfSkey vrfSignKey)
      vrfKey       = skFromBytes vrfKeyBytes
      decodedNonce = decodeBS $ textToBS nonce

      slotsOfEpoch = [firstSlotOfEpoch .. firstSlotOfEpoch + epochLength]
      sigmaOfF     = mkSigmaOfF activeSlotCoeff poolSigma

  mapM_ (isSlotLeader sigmaOfF decodedNonce vrfKey) slotsOfEpoch

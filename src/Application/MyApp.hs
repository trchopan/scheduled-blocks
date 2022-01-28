module Application.MyApp where

import           Data.Aeson                     ( eitherDecode )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.Text                     as T
import           Domain.BlockchainGenesis       ( BlockchainGenesis )
import           Domain.CardanoGlobal           ( ArmdaNonce
                                                , Cardano
                                                , nextEpochs
                                                )
import           Domain.PoolInfo                ( PoolInfo )
import           Infrastructure.Api             ( getBlockchainGenesis
                                                , getEpochParam
                                                , getGlobal
                                                , getPoolInfo
                                                )
import           Network.HTTP.Simple            ( getResponseBody
                                                , httpLBS
                                                )


epochSchedule :: IO ()
epochSchedule = do
  globalResBs <- httpLBS getGlobal
  let eitherGlobal =
        eitherDecode (getResponseBody globalResBs) :: Either String Cardano
  case eitherGlobal of
    Left  err           -> print ("Cannot get global. Err: " ++ err)
    Right cardanoGlobal -> putStr $ unlines $ nextEpochs cardanoGlobal 3


epochBlockSchedule :: String -> String -> Int -> IO ()
epochBlockSchedule blockFrostApi poolId epochNo = do
  let _blockFrostApi = T.pack blockFrostApi

  nonceResBs <- httpLBS $ getEpochParam _blockFrostApi epochNo
  let eitherNonce =
        eitherDecode (getResponseBody nonceResBs) :: Either String ArmdaNonce
  case eitherNonce of
    Left  err   -> print ("No nonce. Err" ++ err)
    Right nonce -> do
      L.putStrLn $ encodePretty nonce

  genesisResBs <- httpLBS $ getBlockchainGenesis _blockFrostApi
  let eitherBlockGenesis =
        eitherDecode (getResponseBody genesisResBs) :: Either
            String
            BlockchainGenesis

  poolInfoBs <- httpLBS $ getPoolInfo _blockFrostApi poolId
  let eitherPoolInfo =
        eitherDecode (getResponseBody poolInfoBs) :: Either String PoolInfo
  case eitherPoolInfo of
    Left  err      -> print ("No poolInfo " ++ err)
    Right poolInfo -> do
      L.putStrLn $ encodePretty poolInfo

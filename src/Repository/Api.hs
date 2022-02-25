module Repository.Api where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( FromJSON
                                                , eitherDecode
                                                )
import           Data.ByteString.UTF8           ( fromString )
import           Network.HTTP.Client.Conduit    ( responseTimeoutMicro )
import           Network.HTTP.Simple            ( Request
                                                , defaultRequest
                                                , getResponseBody
                                                , httpLBS
                                                , setRequestHeader
                                                , setRequestHost
                                                , setRequestMethod
                                                , setRequestPath
                                                , setRequestResponseTimeout
                                                )
import           Network.HTTP.Types             ( Method
                                                , methodGet
                                                )
import           Text.Printf                    ( printf )

toMicroseconds :: Int -> Int
toMicroseconds ms = ms * 1000 * 1000

timeOutSeconds :: Int -> Int
timeOutSeconds = toMicroseconds

handleEitherFailOrResult :: Either String p -> p
handleEitherFailOrResult e = do
  case e of
    Left  err -> error $ printf "Failed to handle result. Error: %s\n" err
    Right v   -> v

requestAndDecode :: (MonadIO m, FromJSON a) => Request -> m a
requestAndDecode request =
  httpLBS request
    >>= return
    .   handleEitherFailOrResult
    .   eitherDecode
    .   getResponseBody

simpleRequest :: Method -> String -> String -> Request
simpleRequest method host path =
  setRequestMethod method
    $ setRequestHost (fromString host)
    $ setRequestPath (fromString path)
    $ setRequestResponseTimeout (responseTimeoutMicro (timeOutSeconds 10))
    $ setRequestHeader "Content-Type" ["application/json"] defaultRequest

simpleGetRequest :: String -> String -> Request
simpleGetRequest = simpleRequest methodGet

adaPoolRequest :: String -> Request
adaPoolRequest = simpleGetRequest "js.adapools.org"

getGlobal :: Request
getGlobal = adaPoolRequest "/global.json"

armdaRequest :: String -> Request
armdaRequest = simpleGetRequest "nonce.armada-alliance.io"

getNextNonce :: Request
getNextNonce = armdaRequest "/next"

getCurrentNonce :: Request
getCurrentNonce = armdaRequest "/current"

blockFrost :: String -> String -> Request
blockFrost apiKey path = do
  setRequestHeader "project_id" [fromString apiKey]
    $  simpleGetRequest "cardano-mainnet.blockfrost.io"
    $  "/api/v0/"
    ++ path

getEpochParam :: String -> Int -> Request
getEpochParam apiKey epoch =
  blockFrost apiKey $ printf "/epochs/%d/parameters" epoch

getEpochInfo :: String -> Int -> Request
getEpochInfo apiKey epoch =
  blockFrost apiKey $ printf "/epochs/%d" epoch


getPoolInfo :: String -> String -> Request
getPoolInfo apiKey poolId =
  blockFrost apiKey $ printf "/pools/%s" poolId

getPoolHistory :: String -> String -> Request
getPoolHistory apiKey poolId =
  blockFrost apiKey $ printf "/pools/%s/history" poolId

getBlockchainGenesis :: String -> Request
getBlockchainGenesis apiKey = blockFrost apiKey "/genesis"

getFirstShellyBlock :: String -> Request
getFirstShellyBlock apiKey = blockFrost apiKey "/blocks/4555184"

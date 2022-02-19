module Infrastructure.Api where

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TSE
import           Network.HTTP.Client.Conduit    ( responseTimeoutMicro )
import           Network.HTTP.Simple            ( Request
                                                , defaultRequest
                                                , setRequestHeader
                                                , setRequestHost
                                                , setRequestMethod
                                                , setRequestPath
                                                , setRequestResponseTimeout
                                                )
import           Text.Printf                    ( printf )

toMicroseconds :: Int -> Int
toMicroseconds ms = ms * 1000 * 1000

myResponseTimeout :: Int
myResponseTimeout = toMicroseconds 10

simpleGetRequest :: T.Text -> T.Text -> Request
simpleGetRequest host path =
  setRequestMethod "GET"
    $ setRequestHost (TSE.encodeUtf8 host)
    $ setRequestPath (TSE.encodeUtf8 path)
    $ setRequestResponseTimeout (responseTimeoutMicro myResponseTimeout)
    $ setRequestHeader "Content-Type" ["application/json"] defaultRequest

adaPoolRequest :: T.Text -> Request
adaPoolRequest = simpleGetRequest "js.adapools.org"

getGlobal :: Request
getGlobal = adaPoolRequest "/global.json"

armdaRequest :: T.Text -> Request
armdaRequest = simpleGetRequest "nonce.armada-alliance.io"

getNextNonce :: Request
getNextNonce = armdaRequest "/next"

getCurrentNonce :: Request
getCurrentNonce = armdaRequest "/current"

blockFrostRequest :: T.Text -> T.Text -> Request
blockFrostRequest apiKey path = do
  setRequestHeader "project_id" [TSE.encodeUtf8 apiKey]
    $ simpleGetRequest "cardano-mainnet.blockfrost.io"
    $ T.concat ["/api/v0", path]

getEpochParam :: T.Text -> Int -> Request
getEpochParam apiKey epoch =
  blockFrostRequest apiKey $ T.pack $ printf "/epochs/%d/parameters" epoch

getEpochInfo :: T.Text -> Int -> Request
getEpochInfo apiKey epoch =
  blockFrostRequest apiKey $ T.pack $ printf "/epochs/%d" epoch


getPoolInfo :: T.Text -> String -> Request
getPoolInfo apiKey poolId =
  blockFrostRequest apiKey $ T.pack $ printf "/pools/%s" poolId

getPoolHistory :: T.Text -> String -> Request
getPoolHistory apiKey poolId =
  blockFrostRequest apiKey $ T.pack $ printf "/pools/%s/history" poolId

getBlockchainGenesis :: T.Text -> Request
getBlockchainGenesis apiKey = blockFrostRequest apiKey "/genesis"

getFirstShellyBlock :: T.Text -> Request
getFirstShellyBlock apiKey = blockFrostRequest apiKey "/blocks/4555184"

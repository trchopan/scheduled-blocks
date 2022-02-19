module Main where

import           Application.MyApp              ( epochBlockSchedule
                                                , epochSchedule
                                                )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.UTF8           ( toString )
import           Infrastructure.Crypto          ( currentNonce
                                                , encodeSlot
                                                , seedLBytes
                                                )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( exitFailure )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Text.Printf                    ( printf )

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Print out the schedule for next fiew epochs
    ["epoch-schedule"] -> epochSchedule

    -- Calculate current epoch
    [blockFrostApi, poolId, epochStr] | [(epochNo, _)] <- reads epochStr ->
      epochBlockSchedule blockFrostApi poolId epochNo

    -- Print help
    _ -> do
      name <- getProgName
      hPutStrLn stderr
        $ printf "usage: %s blockFrostApi poolId epochNumber" name
      exitFailure

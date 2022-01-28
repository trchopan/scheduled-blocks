module Main where

import           Application.MyApp              ( epochBlockSchedule
                                                , epochSchedule
                                                )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( exitFailure )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

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
      hPutStrLn stderr $ "usage: " ++ name ++ " blockFrostApi poolId epochNumber"
      -- hPutStrLn stderr $ "usage: " ++ name ++ " blockFrostApi next"
      exitFailure

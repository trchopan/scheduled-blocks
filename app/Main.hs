module Main where

import           Application.HistoryBlocks      ( HistoryBlocksArgs
                                                  ( HistoryBlocksArgs
                                                  )
                                                , historyBlocks
                                                )
import           Application.NextBlocks         ( NextBlocksArgs(NextBlocksArgs)
                                                , nextBlocks
                                                )
import           Options.Applicative            ( CommandFields
                                                , Mod
                                                , Parser
                                                , ParserInfo
                                                , auto
                                                , command
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , hsubparser
                                                , info
                                                , infoOption
                                                , long
                                                , option
                                                , progDesc
                                                , strOption
                                                )

data Opts = Opts
  { blockFrostApi :: String
  , poolId        :: String
  , vrfSkey       :: String
  , optCommand    :: !Command
  }

data Command
    = HistoryBlocksCmd Int
    | NextBlocksCmd

main :: IO ()
main = do
  opts <- execParser optsParser
  case optCommand opts of
    HistoryBlocksCmd epoch -> historyBlocks
      (HistoryBlocksArgs (blockFrostApi opts) epoch (poolId opts) (vrfSkey opts)
      )
    NextBlocksCmd -> nextBlocks
      (NextBlocksArgs (blockFrostApi opts) (poolId opts) (vrfSkey opts))
 where
  optsParser :: ParserInfo Opts
  optsParser = info
    (helper <*> versionOption <*> programOptions)
    (  fullDesc
    <> progDesc "scheduled-blocks history example"
    <> header
         "scheduled-blocks - a small program for checking the blocks schedule of given epoch on Cardano blockchain"
    )
  versionOption :: Parser (a -> a)
  versionOption = infoOption "0.1.0" (long "version" <> help "Show version")
  programOptions :: Parser Opts
  programOptions =
    Opts
      <$> strOption
            (long "blockFrostApi" <> help
              "The blockfrost API. Obtain from: https://blockfrost.io/"
            )
      <*> strOption (long "poolId" <> help "the pool id")
      <*> strOption (long "vrfSkey" <> help "Vrf SignKey of the pool")
      <*> hsubparser (historyCommand <> nextCommand)

  historyCommand :: Mod CommandFields Command
  historyCommand = command
    "history"
    (info historyOptions
          (progDesc "Check the scheduled blocks history of the pool")
    )
  historyOptions :: Parser Command
  historyOptions = HistoryBlocksCmd
    <$> option auto (long "epoch" <> help "Epoch to query the block schedule")

  nextCommand :: Mod CommandFields Command
  nextCommand = command
    "next"
    (info (pure NextBlocksCmd)
          (progDesc "Check the next epoch scheduled blocks of the pool")
    )

module Grafs.Cli where

import Options.Applicative

data CLIOptions = CLIOptions
                { port :: Int }

cliOptions :: Parser CLIOptions
cliOptions = CLIOptions
          <$> option auto
            (  long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "The port Grafs runs on"
            )

getOpts :: IO CLIOptions
getOpts = execParser opts
  where opts = info (helper <*> cliOptions) $
          fullDesc
          <> header "Generic Registering and Form Service"
          <> progDesc "Run Grafs on given PORT"

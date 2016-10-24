module Main where

import Protolude
import Grafs.Cli
import Grafs.Webserver

main :: IO ()
main = do
    (CLIOptions p) <- getOpts
    runWebserver p

module Main where

import Grafs.Cli
import Grafs.Webserver

main :: IO ()
main = do
    (CLIOptions p) <- getOpts
    runWebserver p

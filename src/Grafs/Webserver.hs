{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Grafs.Webserver where

import Servant
import Servant.HTML.Lucid
import Network.Wai.Handler.Warp

type App = "forms" :> Get '[HTML] String

server :: Server App
server = return "hi"

runWebserver :: Int -> IO ()
runWebserver p = run p $ serve appProxy server
  where appProxy :: Proxy App
        appProxy = Proxy

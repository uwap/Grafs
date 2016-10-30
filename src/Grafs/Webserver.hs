{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Grafs.Webserver where

import Protolude
import Lucid
import Servant
import Servant.HTML.Lucid
import Text.Digestive
import Text.Digestive.Lucid.Html5
import Network.Wai.Handler.Warp
import Grafs.Form

type App = "forms" :> Get '[HTML] Page

data Page = Page

instance ToHtml Page where
  toHtml _ = doctypehtml_ $ do
    head_ $ title_ "testform"
    body_ $ renderForm [ FormField InputText "nick" "Your Nick Name", FormField InputText "realname" "Your Realname" ]

server :: Server App
server = return Page

runWebserver :: Int -> IO ()
runWebserver p = run p $ serve appProxy server
  where appProxy :: Proxy App
        appProxy = Proxy

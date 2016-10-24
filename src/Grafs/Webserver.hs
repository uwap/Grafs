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

type App = "forms" :> Get '[HTML] Page

data Test = Test
          { nick :: Text
          , age  :: Text
          }

testForm :: (Monad m, Monad m1) => Form (HtmlT m1 ()) m Test
testForm = Test
        <$> "nick" .: text Nothing
        <*> "age" .: text Nothing

renderForm :: Monad m => View (HtmlT m ()) -> HtmlT m ()
renderForm v = form v "POST" $ do
  p_ $ do
    label "nick" v "Nick name: "
    inputText "nick" v
  p_ $ do
    label "age" v "Age: "
    inputText "age" v
  inputSubmit "Submit"

data Page = Page

instance ToHtml Page where
  toHtml _ = doctypehtml_ $ do
    head_ $ title_ "testform"
    body_ $ renderForm . runIdentity $ getForm "test" testForm

server :: Server App
server = return Page

runWebserver :: Int -> IO ()
runWebserver p = run p $ serve appProxy server
  where appProxy :: Proxy App
        appProxy = Proxy

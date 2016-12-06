{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Grafs.Webserver where

import Protolude
import Lucid
import qualified Lucid.Base as LUB
import Servant
import Servant.HTML.Lucid
import Text.Digestive
import Text.Digestive.Lucid.Html5
import Network.Wai.Handler.Warp
import Grafs.Form
import Grafs.Validation
import qualified Data.Map as M

type App = "forms" :> Get '[HTML] Page
      :<|> "static" :> Raw
      :<|> "forms2" :> ReqBody '[FormUrlEncoded] [(Text,Text)] :> Post '[HTML] Page
--    :<|> "addform.json" :> ... '[JSON] [FormField] :> Post '[JSON] Success
--    :<|> "addform.html" :> ... '[JSON] [FormField] :> Post '[HTML] Success

data Page = Page (Html ())

-- TODO: Move this somewhere
header :: Monad m => HtmlT m ()
header =
  div_ [ id_ "header" ] $ do
    h1_ "Grafs"
    hr_ []

instance ToHtml Page where
  toHtml (Page x) = doctypehtml_ $ do
    head_ $ do
      title_ "testform"
      link_ [ rel_ "stylesheet", href_ "static/style.css" ]
    body_ $ do
      div_ [ class_ "constraint" ] $ do
        header
        lower x
    where lower :: Monad m => Html a -> HtmlT m a
          lower (LUB.HtmlT (Identity x)) = LUB.HtmlT $ return x

myForm :: Monad m => FormResponse -> (HtmlT m (), Maybe [Text])
myForm x = renderForm x [ FormField InputText "Your Nickname" [NotEmpty]
                        , FormField InputText "Your Real Name" []
                        , FormField (Radio [ "Banana", "Apple", "Grapefruit" ]) "Favourite Fruit" []
                        , FormField InputTextArea "Test" [NotEmpty]
                        ]

server :: Server App
server = formsRequest :<|> staticRequest :<|> forms2Request
  where formsRequest = return . Page . fst $ myForm NoResponse
        staticRequest = serveDirectory "static"
        forms2Request (M.fromList -> t) = let (v, r) = myForm (Response t) in print r >> return (Page v)


runWebserver :: Int -> IO ()
runWebserver p = run p $ serve appProxy server
  where appProxy :: Proxy App
        appProxy = Proxy

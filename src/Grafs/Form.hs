{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Grafs.Form where

import Protolude
import Text.Digestive
import Text.Digestive.Lucid.Html5
import Lucid
import Data.Aeson hiding ((.:))

keynames :: [LText]
keynames = toSL <$> keynames'
  where keynames' = map pure ['a'..'z'] ++ map pure ['A'..'Z'] ++ (map (\x -> (x ++)) keynames' <*> keynames')

data FormType = InputText | InputTextArea | Radio [Text] deriving (Generic, Show, Eq)
instance FromJSON FormType
instance ToJSON FormType

data FormField = FormField
               { formType :: FormType
               , formDesc :: Text
               } deriving (Generic, Show, Eq)
instance FromJSON FormField
instance ToJSON FormField

viewField :: Monad m => Text -> View (HtmlT m ()) -> FormField -> HtmlT m ()
viewField n v = viewField'
  where viewField' (FormField InputText desc) = do
          label n v (toHtml desc) >> br_ []
          inputText n v
        viewField' (FormField InputTextArea desc) = do
          label n v (toHtml desc) >> br_ []
          inputTextArea Nothing Nothing n v
        viewField' (FormField (Radio options) desc) = do
          toHtml desc >> br_ []
          inputRadio True n v

fromFormType :: (Monad m, Monad m1) => FormType -> Form (HtmlT m1 ()) m Text
fromFormType InputText = text Nothing
fromFormType InputTextArea = text Nothing
fromFormType (Radio xs) = choice (zip xs (map toHtml xs)) Nothing

mkForm :: (Monad m, Monad m1) => [(Text, FormField)] -> Form (HtmlT m1 ()) m [Text]
mkForm [] = pure []
mkForm ((formKey, FormField {..}) : xs) = (:) <$> formKey .: fromFormType formType <*> mkForm xs

viewFormFields :: Monad m => [FormField] -> View (HtmlT m ())
viewFormFields = runIdentity . getForm "" . mkForm . zip (map toS keynames)

renderForm :: Monad m => [FormField] -> HtmlT m ()
renderForm ffs = renderForm' $ viewFormFields ffs
  where renderForm' :: Monad m => View (HtmlT m ()) -> HtmlT m ()
        renderForm' v = form v "forms2" $ do
          forM_ (zip keynames ffs) $ \(n,f) -> p_ $ viewField (toS n) v f
          inputSubmit "Submit"

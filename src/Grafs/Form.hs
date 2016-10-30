{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Grafs.Form where

import Protolude
import Text.Digestive
import Text.Digestive.Lucid.Html5
import Lucid

data FormType = InputText

data FormField = FormField
               { formType :: FormType
               , formKey  :: Text
               , formDesc :: Text
               }

fromFormType _ = text Nothing

mkForm :: (Monad m, Monad m1) => [FormField] -> Form (HtmlT m1 ()) m [Text]
mkForm [] = pure []
mkForm (FormField {..} : xs) = (:) <$> formKey .: fromFormType formType <*> mkForm xs

viewFormFields :: Monad m => [FormField] -> View (HtmlT m ())
viewFormFields = runIdentity . getForm "" . mkForm

renderForm :: Monad m => [FormField] -> HtmlT m ()
renderForm ffs = renderForm' $ viewFormFields ffs
  where renderForm' :: Monad m => View (HtmlT m ()) -> HtmlT m ()
        renderForm' v = forM_ ffs $ \(FormField {..}) -> p_ $ do
          label formKey v (toHtml formDesc)
          inputText formKey v

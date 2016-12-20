{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Grafs.Form where

import Protolude hiding (check)
import Text.Digestive
import Text.Digestive.Lucid.Html5
import Lucid
import Data.Aeson hiding ((.:))
import Data.Map (lookup)
import qualified Data.Text as T

import Grafs.Validation

keynames :: [LText]
keynames = toSL <$> keynames'
  where keynames' = map pure ['a'..'z'] ++ map pure ['A'..'Z'] ++ (map (\x -> (x ++)) keynames' <*> keynames')

data FormResponse = NoResponse
                  | Response (Map Text Text)

responseFromMaybe :: Maybe (Map Text Text) -> FormResponse
responseFromMaybe = maybe NoResponse Response

data FormType = InputText
              | InputTextArea
              | Radio [Text]
              | CheckBox [Text]
            deriving (Generic, Show, Eq)
instance FromJSON FormType
instance ToJSON FormType

data FormField = FormField
               { formType          :: FormType
               , formDesc          :: Text
               , formValidations :: [Validation]
               } deriving (Generic, Show, Eq)
instance FromJSON FormField
instance ToJSON FormField

viewField :: Monad m => Text -> View (HtmlT m ()) -> FormField -> HtmlT m ()
viewField n v = viewField'
  where viewField' (FormField InputText desc _) = do
          label n v (toHtml desc) >> br_ []
          errorList n v
          inputText n v
        viewField' (FormField InputTextArea desc _) = do
          label n v (toHtml desc) >> br_ []
          errorList n v
          inputTextArea Nothing Nothing n v
        viewField' (FormField (Radio options) desc _) = do
          toHtml desc >> br_ []
          errorList n v
          inputRadio True n v
        viewField' (FormField (CheckBox options) desc _) = do
          toHtml desc >> br_ []
          errorList n v
          inputCheckbox n v

fromFormType :: (Monad m, Monad m1) => FormType -> Form (HtmlT m1 ()) m Text
fromFormType InputText = text Nothing
fromFormType InputTextArea = text Nothing
fromFormType (Radio xs) = choice (zip xs (map toHtml xs)) Nothing
fromFormType (CheckBox xs) = ??? (zip xs (map toHtml xs)) Nothing

mkForm :: (Monad m, Monad m1) => [(Text, FormField)] -> Form (HtmlT m1 ()) m [Text]
mkForm [] = pure []
mkForm ((formKey, FormField {..}) : xs) = (:) <$> formKey .: checkAll formValidations (fromFormType formType) <*> mkForm xs
  where checkAll xs = foldr' (.) (\x -> x) (runValidation <$> xs) 

viewFormFields :: Monad m => FormResponse -> [FormField] -> (View (HtmlT m ()), Maybe [Text])
viewFormFields NoResponse = (, Nothing) . runIdentity . getForm "" . mkForm . zip (map toS keynames)
viewFormFields (Response m) = runIdentity . flip (postForm "") env . mkForm . zip (map toS keynames)
  where env = const . Identity $ \p -> Identity $ maybeToList $ TextInput <$> lookup (fromPath p) m

renderForm :: Monad m => FormResponse -> [FormField] -> (HtmlT m (), Maybe [Text])
renderForm req ffs = let (v, r) = viewFormFields req ffs in (renderForm' v, r)
  where renderForm' :: Monad m => View (HtmlT m ()) -> HtmlT m ()
        renderForm' v = form v "forms2" $ do
          forM_ (zip keynames ffs) $ \(n,f) -> with div_ [ class_ "formelem" ] $ viewField (toS n) v f
          inputSubmit "Submit"

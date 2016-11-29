{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Grafs.Validation where

import Protolude hiding (check)
import qualified Data.Text as T
import Data.Aeson
import Text.Digestive
import Lucid

data Validation = NotEmpty
                deriving (Eq, Show, Generic)
instance FromJSON Validation
instance ToJSON Validation

runValidation :: (Monad m0, Monad m1) => Validation -> (Form (HtmlT m0 ()) m1 Text -> Form (HtmlT m0 ()) m1 Text)
runValidation NotEmpty = check "Input may not be empty" (not . T.null)

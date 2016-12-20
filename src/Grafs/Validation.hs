{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Grafs.Validation where

import Protolude hiding (check)
import qualified Data.Text as T
import Data.Aeson
import Text.Digestive
import Lucid

data Validation = NotEmpty
                | LessThan Integer
                | GreaterThan Integer
                | ShorterThan Int
                | LongerThan Int
                | NotEqualTo Text
                | Email
                deriving (Eq, Show, Generic)
instance FromJSON Validation
instance ToJSON Validation

validationErrorMessage :: Validation -> Text
validationErrorMessage NotEmpty         = "Input may not be empty"
validationErrorMessage (GreaterThan i)  = "Input must be greater than " <> show i
validationErrorMessage (LessThan i)     = "Input must be less than " <> show i
validationErrorMessage (ShorterThan i)  = "Input must be shorter than " <> show i <> " characters"
validationErrorMessage (LongerThan i)   = "Input must be longer than " <> show i <> " characters"
validationErrorMessage (NotEqualTo t)   = "Input must not be " <> t
validationErrorMessage Email            = "Input must be a valid email address"

runValidation :: (Monad m0, Monad m1) => Validation -> (Form (HtmlT m0 ()) m1 Text -> Form (HtmlT m0 ()) m1 Text)
runValidation v = check (toHtml $ validationErrorMessage v) $ f v
  where f NotEmpty        = (not . T.null)
        f (GreaterThan i) = (any (> i) . readMaybe . toS)
        f (LessThan i)    = (any (< i) . readMaybe . toS)
        f (ShorterThan i) = ((< i) . T.length)
        f (LongerThan i)  = ((> i) . T.length)
        f (NotEqualTo t)  = (/= t)
        f Email           = T.isInfixOf "@"

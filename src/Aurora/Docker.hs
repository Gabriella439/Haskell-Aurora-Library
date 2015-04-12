{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Aurora.Docker where

import Control.Applicative (pure)
import Data.Text (Text)
import Text.PrettyPrint.Leijen (Pretty(..))

import Aurora.Pretty (recordDoc, text)

data Docker = Docker
    { image :: Text
    } deriving (Eq, Show)

{-| Default `Docker`

    Required fields: `image`
-}
_Docker :: Docker
_Docker = Docker {}

instance Pretty Docker where
    pretty (Docker{..}) = recordDoc "Docker"
        [ ("image", fmap text (pure image))
        ]

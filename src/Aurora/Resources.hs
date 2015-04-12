{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Aurora.Resources where

import Control.Applicative (pure)
import Text.PrettyPrint.Leijen (Pretty(..), double, integer)

import Aurora.Pretty (recordDoc)

data Resources = Resources
    { cpu  :: Double
    , ram  :: Integer
    , disk :: Integer
    } deriving (Eq, Show)

instance Pretty Resources where
    pretty (Resources{..}) = recordDoc "Resources"
        [ ("cpu" , fmap double  (pure cpu ))
        , ("ram" , fmap integer (pure ram ))
        , ("disk", fmap integer (pure disk))
        ]

{-| Default `Resources`

    Required fields: `cpu`, `ram`, and `disk`
-}
_Resources :: Resources
_Resources = Resources {}

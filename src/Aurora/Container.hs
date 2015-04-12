{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Aurora.Container where

import Control.Applicative (pure)
import Text.PrettyPrint.Leijen (Pretty(..))

import Aurora.Docker (Docker)
import Aurora.Pretty (recordDoc)

data Container = Container
    { docker :: Docker
    } deriving (Eq, Show)

{-| Default `Container`

    Required fields: `docker`
-}
_Container :: Container
_Container = Container {}

instance Pretty Container where
    pretty (Container{..}) = recordDoc "Container"
        [ ("docker", fmap pretty (pure docker))
        ]

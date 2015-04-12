{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Aurora.Process where

import Control.Applicative (empty, pure)
import Data.Text (Text)
import Text.PrettyPrint.Leijen (Pretty(..), bool, integer)

import Aurora.Optional (Optional)
import Aurora.Pretty (recordDoc, text)

data Process = Process
    { name         :: Text
    , cmdline      :: Text
    , max_failures :: Optional Integer
    , daemon       :: Optional Bool
    , ephemeral    :: Optional Bool
    , min_duration :: Optional Integer
    , final        :: Optional Bool
    } deriving (Eq, Show)

instance Pretty Process where
    pretty (Process{..}) = recordDoc "Process"
        [ ("name"        , fmap text    (pure name        ))
        , ("cmdline"     , fmap text    (pure cmdline     ))
        , ("max_failures", fmap integer (     max_failures))
        , ("daemon"      , fmap bool    (     daemon      ))
        , ("ephemeral"   , fmap bool    (     ephemeral   ))
        , ("min_duration", fmap integer (     min_duration))
        , ("final"       , fmap bool    (     final       ))
        ]

{-| Default `Process`

    Required fields: `processName` and `cmdline`
-}
_Process :: Process
_Process = Process
    { max_failures = empty
    , daemon       = empty
    , ephemeral    = empty
    , min_duration = empty
    , final        = empty
    }
